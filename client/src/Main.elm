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

        BuyCap _ ->
            Debug.todo "y u no do this"

        CancelBuyCap ->
            ( { model | buyingCap = Nothing }, Cmd.none )

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


getTotalOwed : List Purchase -> Int
getTotalOwed purchases =
    List.foldr (\purchase sum -> sum + purchase.cents) 0 purchases


backgroundColor : E.Color
backgroundColor =
    E.rgb255 87 86 195


view : Model -> Html Msg
view model =
    let
        baseAttrs =
            [ E.width E.fill
            , E.height E.fill
            , E.centerX
            , BG.gradient
                { angle = pi / 4, steps = [ backgroundColor, E.rgb255 255 255 255 ] }
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
            [ el [] <| text "How much have you spent on cappuccinos?"
            , el [] <| text <| centsToString <| model.capSum
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
            E.column [ E.centerX ]
                [ el [ E.width (E.px 100), E.height (E.px 100) ]
                    (E.html <| capSvg shop.lidColor)
                , el
                    [ E.centerX
                    , E.moveLeft 10
                    , Font.color (E.rgb255 255 255 255)
                    ]
                    (text shop.name)
                , el
                    [ E.centerX
                    , E.moveLeft 10
                    , Font.color (E.rgb255 255 255 255)
                    ]
                  <|
                    text <|
                        centsToString shop.priceCents
                ]
        }


confirmDialog : CoffeeShop -> E.Element Msg
confirmDialog ({ name, priceCents } as shop) =
    let
        modalDimension =
            E.px 150
    in
    el [ E.width modalDimension, E.height modalDimension ] <|
        E.column []
            [ E.text <|
                "Are you sure you would like to purchase "
                    ++ name
                    ++ " from "
                    ++ centsToString priceCents
                    ++ "?"
            , E.row []
                [ button []
                    { onPress = Just <| BuyCap shop
                    , label = E.text "Confirm"
                    }
                , button []
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
