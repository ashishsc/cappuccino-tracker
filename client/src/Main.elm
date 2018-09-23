module Main exposing (Model, Msg(..), NewPurchase, Purchase, init, main, update, view)

import Browser
import Element exposing (el, text)
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


type CoffeeShop
    = JaHo
    | Prett


coffeeShopPrice : CoffeeShop -> Int
coffeeShopPrice shop =
    case shop of
        JaHo ->
            900

        Prett ->
            600


coffeeShopToString : CoffeeShop -> String
coffeeShopToString shop =
    case shop of
        JaHo ->
            "JaHo"

        Prett ->
            "Prett"


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
    }


init : ( Model, Cmd Msg )
init =
    ( { purchases = []
      , newPurchaseDescription = ""
      , newPurchaseCents = 0
      , capSum = 0

      -- TODO Set this to True and render some nice loading
      , isLoading = False
      }
    , Cmd.batch [ getPurchasesCmd, getCapSumCmd ]
    )


type Msg
    = NewDescUpdated String
    | NewCentsUpdated Int
    | BuyCap CoffeeShop
    | PurchasesFetched (Result Http.Error (List Purchase))
    | CapPurchasesSumFetched (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCentsUpdated cents ->
            ( { model | newPurchaseCents = cents }, Cmd.none )

        NewDescUpdated desc ->
            ( { model | newPurchaseDescription = desc }, Cmd.none )

        BuyCap _ ->
            Debug.todo "y u no do this"

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


view : Model -> Html Msg
view model =
    Element.column []
        [ el [] <| text "How many cappuccinos do you owe me?"
        , el [] <| text <| String.fromInt <| getTotalOwed <| model.purchases
        , el [] <| text "How much have you spent on cappuccinos?"
        , el [] <| text <| String.fromInt <| model.capSum
        , Element.row [] [ capView JaHo, capView Prett ]
        ]
        |> Element.layout []


capView : CoffeeShop -> Element.Element Msg
capView shop =
    button []
        { onPress = Just (BuyCap JaHo)
        , label =
            Element.image [ Element.width (Element.px 50), Element.height (Element.px 50) ]
                { src = "cappuccino.svg"
                , description = "Buy cappuccino from " ++ coffeeShopToString shop
                }
        }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
