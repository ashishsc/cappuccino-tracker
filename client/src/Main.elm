module Main exposing (Model, Msg(..), NewPurchase, Purchase, init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, img, text)
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


{-| TODO: get this from env
-}
apiUrl : Url.Url
apiUrl =
    { protocol = Url.Http
    , host = "localhost"
    , port_ = Just 6969
    , path = ""
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
    }


init : ( Model, Cmd Msg )
init =
    ( { purchases = []
      , newPurchaseDescription = ""
      , newPurchaseCents = 0
      }
    , getPurchasesCmd
    )


type Msg
    = NewDescUpdated String
    | NewCentsUpdated Int
    | CreateNewPurchase
    | PurchasesFetched (Result Http.Error (List Purchase))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewCentsUpdated cents ->
            ( { model | newPurchaseCents = cents }, Cmd.none )

        NewDescUpdated desc ->
            ( { model | newPurchaseDescription = desc }, Cmd.none )

        CreateNewPurchase ->
            Debug.todo "y u no do this"

        PurchasesFetched res ->
            case res of
                Err err ->
                    Debug.log (Debug.toString err) ( model, Cmd.none )

                Ok purchases ->
                    ( { model | purchases = purchases }, Cmd.none )


getTotalOwed : List Purchase -> Int
getTotalOwed purchases =
    List.foldr (\purchase sum -> sum + purchase.cents) 0 purchases


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "How many cappuccinos do you owe me?" ]
        , div [ class "total-owed" ] [ text <| String.fromInt <| getTotalOwed <| model.purchases ]
        , div [ class "shop-choices" ]
            [ div [ class "shop" ] [ button [] [ text "Jaho" ] ]
            , div [ class "shop" ] [ button [] [ text "Press" ] ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
