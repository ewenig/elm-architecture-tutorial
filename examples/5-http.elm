import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import String exposing (isEmpty)


main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , error : String
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" ""
  , getRandomGif topic
  )



-- UPDATE


type Msg
  = MorePlease
  | UpdateTopic String
  | FetchSucceed String
  | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)

    UpdateTopic newTopic ->
      ( { model | topic = newTopic }, Cmd.none)

    FetchSucceed newUrl ->
      ( { model | gifUrl = newUrl, error = "" }, Cmd.none)

    FetchFail err ->
      let
        errtext =
          case err of
            Http.Timeout ->
              "HTTP request timed out"

            Http.NetworkError ->
              "Network failure"

            Http.UnexpectedPayload _ ->
              "Couldn't decode JSON"

            Http.BadResponse code _ ->
              "Unexpected HTTP response code " ++ toString code
          
      in
        ( { model | error = errtext } , Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", value model.topic, onInput UpdateTopic ] []
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    , h1 [ style [ ("color", "red") ] ] [ if not ( String.isEmpty model.error ) then text ( "Error: " ++ model.error ) else text "" ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "//api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)


decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string
