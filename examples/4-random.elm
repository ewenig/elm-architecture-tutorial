import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Random
import List exposing (head, drop)
import Maybe exposing (withDefault)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { dieFaces : List Int
  }


init : (Model, Cmd Msg)
init =
  (Model [1,1], Cmd.none)



-- UPDATE


type Msg
  = Roll
  | NewFace ( List Int )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      -- Generate a list with two random ints
      ( model, Random.generate NewFace ( Random.list 2 ( Random.int 1 6 ) ) )

    NewFace newFace ->
      ( { dieFaces = newFace }, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
-- gah, lists are not the best way to solve this problem (slightly
-- unwieldy). Still, it gets the job done with only 2 elements.
view model =
  div []
    [ h1 [] [ text (toString ( Maybe.withDefault 0 ( List.head model.dieFaces ) ) ++ " - " ++ toString ( withDefault 0 ( List.head ( List.drop 1 model.dieFaces ) ) ) ) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
