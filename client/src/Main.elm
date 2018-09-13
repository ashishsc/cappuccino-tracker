module Main exposing (Model, Msg(..), NewPurchase, Purchase, init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode


type alias Purchase =
    { id : Int
    , description : String
    , cents : Int
    }


purchaseDecoder : Decode.Decoder Purchase
purchaseDecoder =
    Decode.map3 Purchase
        (Decode.field "id" Decode.int)
        (Decode.field "description" Decode.string)
        (Decode.field "cents" Decode.int)


getPurchases : Http.Request (List Purchase)
getPurchases =
    Http.get "localhost:6969/all" (Decode.list purchaseDecoder)


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
                    Debug.todo (Debug.toString err)

                Ok purchases ->
                    ( { model | purchases = purchases }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
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
