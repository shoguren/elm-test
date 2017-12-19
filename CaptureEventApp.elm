module CaptureEventApp exposing (..)

import Date exposing (Date)
import Html exposing (Attribute, Html, button, div, input, label, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { eventName : String
    , events : List String
    }


type alias Event =
    { id : Int
    , eventName : Maybe String
    , eventDate : Maybe Date
    }


model : Model
model =
    { eventName = "Hiya"
    , events =
        [ "Evento 1"
        , "Evento 2"
        ]
    }



-- UPDATE


type Msg
    = Change String
    | AddEvent
    | RemoveEvent String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | eventName = newContent }

        AddEvent ->
            { model | events = (model.eventName :: model.events), eventName = "" }

        RemoveEvent eventName ->
            { model | events = (List.filter (\e -> e /= eventName) model.events) }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to reverse", value model.eventName, onInput Change ] []
        , button [ onClick AddEvent ] [ text "Add Event" ]
        , div [] [ text (String.reverse model.eventName) ]
        , ul []
            (List.map (\description -> eventView description) model.events)
        ]


eventView : String -> Html Msg
eventView description =
    li []
        [ button [ onClick (RemoveEvent description) ] [ text "X" ]
        , text description
        ]
