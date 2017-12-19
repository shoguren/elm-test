port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random
import Http
import Html.Attributes exposing (id, class, classList, src, name, type_, title)
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
--import Debug
--import Slider exposing (paperSlider)

photoDecoder : Decoder Photo
photoDecoder = decode Photo
    |> required "url" string
    |> required "size" int
    |> optional "title" string "(Untitled)"

--buildPhoto : String -> Int -> String -> Photo
--buildPhoto url size title = { url = url, size = size, title = title }

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

type ThumbnailSize
    = Small
    | Medium
    | Large

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
--        , button [ onClick { operation = "SELECT_RANDOM", data = "" } ] [text "Surprise me!"]
        , button [ onClick SurpriseMe ] [text "Surprise Me!"]
        , div [class "filters" ]
            [ viewFilter SetHue "Hue" model.hue
            , viewFilter SetRipple "Ripple" model.ripple
            , viewFilter SetNoise "Noise" model.noise
            ]

        , div [ id "choose-size" ] (List.map viewSizeChooser [Small, Medium, Large])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ] (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
   div  [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Html.Attributes.max "11", onImmediateValueChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]

viewLarge : Maybe String -> Html Msg
viewLarge selectedUrl =
    case selectedUrl of
        Nothing -> text ""
        Just url ->
--         img [ class "large", src (urlPrefix ++ "large/" ++ url) ] []
         canvas [ id "main-canvas", class "large" ] []

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail  =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]")
        , classList [("selected", selectedUrl == Just thumbnail.url)]
--        , onClick { operation="SELECT_PHOTO", data = thumbnail.url }
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [type_ "radio", name "size", onClick (SetSize size)] []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small -> "small"

        Medium -> "med"

        Large -> "large"


type alias Photo = {url:String, size:Int, title:String}

type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }

initialModel : Model
initialModel =
    {   photos =     []
    ,   selectedUrl = Nothing
    , loadingError = Just "hola"
    ,   chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    }


getPhotoUrl : Model -> Int -> Maybe String
getPhotoUrl model index =
--    case Array.get index (photoArray model) of
--        Just photo -> Just photo.url
--        Nothing -> Nothing

--    Maybe.map (\photo -> photo.url) (Array.get index (photoArray model))
--    Maybe.map .url (Array.get index (Array.fromList model.photos))
    model.photos
        |> Array.fromList
        |> Array.get index
        |> Maybe.map .url


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))
--    | LoadPhotos (Result Http.Error String)
--    {operation:String, data:String}
    | SetHue Int
    | SetRipple Int
    | SetNoise Int

randomPhotoPicker : List Photo -> Random.Generator Int
randomPhotoPicker list = Random.int 0 (List.length list - 1)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetHue hue ->
            applyFilters { model | hue = hue }
        SetRipple ripple ->
            applyFilters { model | ripple = ripple }
        SetNoise noise ->
            applyFilters { model | noise = noise }

        SelectByIndex index -> applyFilters {model | selectedUrl = (getPhotoUrl model index)}

        SelectByUrl url -> applyFilters {model | selectedUrl = Just url}

        SurpriseMe -> (model, selectRandom model)

        SetSize size -> ({model | chosenSize = size}, Cmd.none)

        LoadPhotos (Ok newPhotos) ->
            applyFilters {model | photos = newPhotos
            , selectedUrl = Maybe.map .url (List.head newPhotos)
            , loadingError = Nothing}

--            let
--                urls = (String.split "," okResponse)
--                newPhotos = List.map Photo urls -- Photo == (\url -> { url = url })
----                        Photo "1.jpeg" == { url = "1.jpeg" }
--            in ({model | photos = newPhotos
--                , selectedUrl = List.head urls
--                , loadingError = Nothing}, Cmd.none)
        LoadPhotos (Err httpError) ->
            ({model | loadingError = Just "Error! Server not found!"
             , selectedUrl = Nothing
             , photos = []}, Cmd.none)



--    case msg.operation of
--        "SELECT_PHOTO" ->
--            {model | selectedUrl = msg.data}
--        "SELECT_RANDOM" ->
--            {model | selectedUrl = (selectRandom)}
--        _ ->
--            model

--    if msg.operation == "SELECT_PHOTO" then
--        {model | selectedUrl = msg.data}
--    else if msg.operation == "SELECT_RANDOM" then
--        {model | selectedUrl = (selectRandom)}
--    else
--        model

selectRandom : Model -> Cmd Msg
selectRandom model = Random.generate SelectByIndex (randomPhotoPicker model.photos)
--    "2.jpeg"


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos

--    "http://elm-in-action.com/photos/list"
--        |> Http.getString
--        |> Http.send LoadPhotos

--    Http.send
--    (\result -> LoadPhotos result)
--    (Http.getString "http://elm-in-action.com/photos/list")


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing -> view model
        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [text errorMessage]
                ]

main : Program Never Model Msg
main =
    Html.program
        { init = (initialModel, initialCmd)
        , view = viewOrError
        , update = update
        , subscriptions = \_ -> Sub.none
        }

paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider = node "paper-slider"

onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
--    Debug.log "value" toMsg
    at ["target", "immediateValue"] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"


port setFilters : FilterOptions -> Cmd msg

type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }

applyFilters: Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
         Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]
                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )
         Nothing ->
            ( model, Cmd.none )


