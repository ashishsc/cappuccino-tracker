module Main exposing (Model, Msg(..), NewPurchase, Purchase, init, main, update, view)

import Browser
import Cappuccino exposing (capSvg)
import Element as E exposing (el, text)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Palette
    exposing
        ( backgroundColor
        , black
        , debt
        , grayBlue
        , money
        , textWithBorder
        , white
        )
import Url


type alias Purchase =
    { id : Int
    , description : String
    , cents : Int
    }


type alias CoffeeShop =
    { name : String
    , priceCents : Int
    , lidColor : String
    }


jaHo : CoffeeShop
jaHo =
    CoffeeShop "JaHo" 900 "f79be8"


prett : CoffeeShop
prett =
    CoffeeShop "Prett" 600 "b20101"


{-| TODO: get this from env
-}
apiUrl : Url.Url
apiUrl =
    { protocol = Url.Http
    , host = "localhost"
    , port_ = Just 6969
    , path = "/all"
    , query = Nothing
    , fragment = Nothing
    }


purchaseDecoder : Decode.Decoder Purchase
purchaseDecoder =
    Decode.map3 Purchase
        (Decode.field "id" Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "cents" Decode.int)


getPurchases : Http.Request (List Purchase)
getPurchases =
    Http.get (Url.toString apiUrl) (Decode.list purchaseDecoder)


getCapSum : Http.Request Int
getCapSum =
    Http.get ({ apiUrl | path = "/cap-total" } |> Url.toString) Decode.int


getCapSumCmd : Cmd Msg
getCapSumCmd =
    getCapSum |> Http.send CapPurchasesSumFetched


getPurchasesCmd : Cmd Msg
getPurchasesCmd =
    getPurchases |> Http.send PurchasesFetched


post : String -> Http.Body -> Http.Request String
post url body =
    Http.request
        { method = "POST"
        , url = url
        , headers = []
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


buyCapCmd : CoffeeShop -> Cmd Msg
buyCapCmd { priceCents, name } =
    let
        endpoint =
            { apiUrl | path = "/buy-cap" } |> Url.toString

        encodedShop =
            Encode.object
                [ ( "shop", Encode.string name )
                , ( "cents", Encode.int priceCents )
                ]
    in
    Http.send CapBought <|
        post endpoint (Http.jsonBody encodedShop)


centsToString : Int -> String
centsToString cents =
    "$ " ++ (toFloat cents / 100 |> String.fromFloat)


type alias NewPurchase =
    { description : String
    , cents : Int
    }


type alias Model =
    { purchases : List Purchase
    , newPurchaseDescription : String
    , newPurchaseCents : Int
    , capSum : Int
    , isLoading : Bool
    , buyingCap : Maybe CoffeeShop
    }


init : ( Model, Cmd Msg )
init =
    ( { purchases = []
      , newPurchaseDescription = ""
      , newPurchaseCents = 0
      , capSum = 0

      -- TODO Set this to True and render some nice loading
      , isLoading = False
      , buyingCap = Nothing
      }
    , Cmd.batch [ getPurchasesCmd, getCapSumCmd ]
    )


type Msg
    = NewDescUpdated String
    | NewCentsUpdated Int
    | BuyCap CoffeeShop
    | ConfirmBuyCap CoffeeShop
    | CancelBuyCap
    | CapBought (Result Http.Error String)
    | PurchasesFetched (Result Http.Error (List Purchase))
    | CapPurchasesSumFetched (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCentsUpdated cents ->
            ( { model | newPurchaseCents = cents }, Cmd.none )

        NewDescUpdated desc ->
            ( { model | newPurchaseDescription = desc }, Cmd.none )

        ConfirmBuyCap shop ->
            ( { model | buyingCap = Just shop }, Cmd.none )

        BuyCap shop ->
            -- Confirmed purchase of a cap
            ( { model | buyingCap = Nothing }
            , buyCapCmd shop
            )

        CancelBuyCap ->
            ( { model | buyingCap = Nothing }, Cmd.none )

        CapBought res ->
            case res of
                Err err ->
                    Debug.log (Debug.toString err) ( model, Cmd.none )

                Ok _ ->
                    ( model, getCapSumCmd )

        PurchasesFetched res ->
            case res of
                Err err ->
                    Debug.log (Debug.toString err) ( model, Cmd.none )

                Ok purchases ->
                    ( { model | purchases = purchases }, Cmd.none )

        CapPurchasesSumFetched res ->
            case res of
                Err err ->
                    Debug.log (Debug.toString err) ( model, Cmd.none )

                Ok sum ->
                    ( { model | capSum = sum }, Cmd.none )


totalOwed : Model -> E.Element Msg
totalOwed { capSum, purchases } =
    let
        totalPurchased =
            List.foldr
                (\purchase sum -> sum + purchase.cents)
                0
                purchases

        total =
            totalPurchased - capSum

        color =
            if total >= 0 then
                debt

            else
                money
    in
    E.row [ E.width (E.px 100) ]
        [ el [] (text "Total Owed ")
        , el [ Font.color color ] (text <| centsToString total)
        ]


view : Model -> Html Msg
view model =
    let
        baseAttrs =
            List.concat
                [ textWithBorder
                , [ E.width E.fill
                  , E.height E.fill
                  , E.centerX
                  , BG.gradient
                        { angle = pi / 2
                        , steps =
                            [ backgroundColor
                            , black
                            , backgroundColor
                            ]
                        }
                  ]
                ]

        attrs =
            case model.buyingCap of
                Just shop ->
                    baseAttrs ++ [ E.inFront (confirmDialog shop) ]

                Nothing ->
                    baseAttrs
    in
    E.layout attrs <|
        E.column
            [ E.width E.fill
            , E.height E.fill
            , E.centerX
            , E.centerY
            , E.padding 50
            ]
            [ totalOwed model
            , E.row [ E.width E.fill, E.height E.fill ]
                [ capView [] jaHo
                , el [ Border.width 2, Border.solid, E.centerX, E.height E.fill ]
                    (text "")
                , capView [] prett
                ]
            ]


capView : List (E.Attribute Msg) -> CoffeeShop -> E.Element Msg
capView attrs shop =
    button attrs
        { onPress = Just (ConfirmBuyCap shop)
        , label =
            E.column [ E.centerX, E.spacing 5 ]
                [ el [ E.width (E.px 100), E.height (E.px 100) ]
                    (E.html <| capSvg shop.lidColor)
                , el [ E.centerX, E.moveLeft 10 ]
                    (text shop.name)
                , el [ E.centerX, E.moveLeft 10, Font.color money ]
                    (text <| centsToString shop.priceCents)
                ]
        }


confirmDialog : CoffeeShop -> E.Element Msg
confirmDialog ({ name, priceCents } as shop) =
    el
        [ E.width (E.px 350)
        , E.height (E.px 100)
        , E.centerX
        , E.centerY
        , BG.color grayBlue
        , E.padding 10
        , Border.rounded 5
        ]
    <|
        E.column [ E.spacing 10, E.centerX ]
            [ E.text <|
                "Confirm purchase from "
                    ++ name
                    ++ "?"
            , E.text ("Price: " ++ centsToString priceCents)
            , E.row [ E.width E.fill, E.spacing 20 ]
                [ button [ E.centerX ]
                    { onPress = Just <| BuyCap shop
                    , label = E.text "Confirm"
                    }
                , button [ E.centerX ]
                    { onPress = Just <| CancelBuyCap
                    , label = E.text "Cancel"
                    }
                ]
            ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
