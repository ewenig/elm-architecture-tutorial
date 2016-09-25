import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type State = Play | Pause
type alias Model =
  { time : Time
  , state : State
  }


init : (Model, Cmd Msg)
init =
  (Model 0 Play, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | PlayPause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({ model | time = newTime }, Cmd.none)

    PlayPause ->
      case model.state of
        Play ->
          ({ model | state = Pause }, Cmd.none)

        Pause ->
          ({ model | state = Play }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
    Play ->
      Time.every second Tick

    Pause ->
      Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model.time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "0 0 100 200", width "300px" ]
      ( [
        circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
      , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
      ] ++ playPauseIcon model ++
      [ rect [ x "46", y "100", width "8", height "10", fillOpacity "0.0", onClick PlayPause ] []
      ] )


playPauseIcon : Model -> List (Svg Msg)
playPauseIcon model =
  case model.state of
    Play ->
      [ rect [ x "46", y "100", width "3", height "10", fill "#666699" ] []
      , rect [ x "51", y "100", width "3", height "10", fill "#666699" ] []
      ]

    Pause ->
      [ polygon [ fill "#666699", points "46,100 54,105 46,110" ] [] ]

