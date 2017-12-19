module FruitsApp exposing (..)

import Html exposing (Html, button, div, span, text, fieldset, label, input, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import FruitsSelection as FS


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = defaults, view = view, update = update }



-- MODEL


type alias Model =
    { fruits : List String
    , selected : FS.SelectedFruits
    }


defaults : Model
defaults =
    { fruits = [ "Grapes", "Banana", "Apple", "Mango", "Orange" ]
    , selected = FS.empty 3
    }



-- UPDATE


type Msg
    = SelectFruit String
    | DeselectFruit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectFruit fruit ->
            { model | selected = FS.insert fruit model.selected }

        DeselectFruit fruit ->
            { model | selected = FS.remove fruit model.selected }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Which fruits do you want?" ]
        , fieldset []
            (List.map (fruitCheckbox model.selected) model.fruits)
        ]


fruitCheckbox : FS.SelectedFruits -> String -> Html Msg
fruitCheckbox selectedFruits fruit =
    let
        isSelected =
            FS.member fruit selectedFruits

        action =
            if isSelected then
                DeselectFruit fruit
            else
                SelectFruit fruit
    in
        checkboxView isSelected fruit action


checkboxView : Bool -> String -> Msg -> Html Msg
checkboxView isChecked description msg =
    label [ style [ ( "display", "block" ) ] ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text description
        ]
