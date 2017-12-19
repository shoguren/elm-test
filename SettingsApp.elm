module SettingsApp exposing (..)

import Html exposing (Html, button, div, span, text, fieldset, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = defaults, view = view, update = update }



-- MODEL


type alias Model =
    { email : Bool
    , autoPlay : AutoPlay
    , location : Bool
    }


type AutoPlay
    = On AutoSettings
    | Off


type alias AutoSettings =
    { sound : Bool, withoutWifi : Bool }


defaults : Model
defaults =
    Model True Off True



-- UPDATE


type Msg
    = ToggleEmail
    | ToggleAutoPlay
    | ToggleAutoPlaySound
    | ToggleAutoPlayWithoutWiFi
    | ToggleLocation


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleEmail ->
            { model | email = not model.email }

        ToggleAutoPlay ->
            case model.autoPlay of
                Off ->
                    { model | autoPlay = On (AutoSettings False False) }

                On _ ->
                    { model | autoPlay = Off }

        ToggleAutoPlaySound ->
            case model.autoPlay of
                Off ->
                    { model | autoPlay = On (AutoSettings True False) }

                On autoSettings ->
                    let
                        newSoundValue =
                            not autoSettings.sound
                    in
                        { model | autoPlay = On { autoSettings | sound = newSoundValue } }

        ToggleAutoPlayWithoutWiFi ->
            case model.autoPlay of
                Off ->
                    { model | autoPlay = On (AutoSettings False True) }

                On autoSettings ->
                    let
                        newWifiValue =
                            not autoSettings.withoutWifi
                    in
                        { model | autoPlay = On { autoSettings | withoutWifi = newWifiValue } }

        ToggleLocation ->
            { model | location = not model.location }



--asDirectorIn : AutoSettings -> Bool -> AutoSettings
--asDirectorIn autoplay newValue =
--    { autoplay | withoutWiFi = newValue }
-- VIEW


view : Model -> Html Msg
view { email, autoPlay, location } =
    let
        autoplayBoolean =
            case autoPlay of
                Off ->
                    False

                On _ ->
                    True

        soundBoolean =
            case autoPlay of
                Off ->
                    False

                On autoSettings ->
                    autoSettings.sound

        wifiBoolean =
            case autoPlay of
                Off ->
                    False

                On autoSettings ->
                    autoSettings.withoutWifi
    in
        fieldset []
            [ checkboxView email "Email Notifications" ToggleEmail
            , checkboxView autoplayBoolean "Video Autoplay" ToggleAutoPlay
            , div []
                [ checkboxView soundBoolean "Play Sound" ToggleAutoPlaySound
                , checkboxView wifiBoolean "Wifi only" ToggleAutoPlayWithoutWiFi
                ]
            , checkboxView location "Use Location" ToggleLocation
            ]


checkboxView : Bool -> String -> Msg -> Html Msg
checkboxView isChecked description msg =
    label []
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text description
        ]
