import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import List exposing (head, map)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  }


model : Model
model =
  Model "" "" "" ""



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , node "br" [] []
    , input [ type' "text", placeholder "Age", onInput Age ] []
    , node "br" [] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , node "br" [] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
    -- make sure passwords match
      if model.password /= model.passwordAgain then
        ("red", "Passwords do not match!")
    -- make sure passwords are appropriate length
      else if String.length model.password <= 8 then
        ("red", "Password must be longer than 8 characters!")
    -- make sure that password meets complexity requirements
      else if not ( validateComplexity model.password ) then
        ("red", "Password must contain upper-case, lower-case and numeric characters")
    -- make sure Age is a number
      else if isErr ( String.toInt model.age ) then
        ("red", "Age must be a number")
    -- if everything passes validation:
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]

validateComplexity : String -> Bool
validateComplexity password =
  let
    upperCaseCharacters = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]
    lowerCaseCharacters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
  in
    if not ( listInString password ( map toString [0..9] ) ) then
       False
    else if not ( listInString password upperCaseCharacters ) then
       False
    else if not ( listInString password lowerCaseCharacters ) then
       False
    else 
       True

listInString : String -> List String -> Bool
listInString string list =
  List.member True ( List.map (flip String.contains string) list )

isErr : Result a b -> Bool
isErr result =
    case result of
        Ok _ -> False
        Err _ -> True

