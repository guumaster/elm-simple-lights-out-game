module LightsGame exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrix exposing (..)


type alias Model =
    { isOn : Board }


type alias Board =
    Matrix (Maybe Bool)


defaultBoard : Board
defaultBoard =
    Matrix.repeat 4 4 (Just True)
        |> setNothing (cell 1 1)
        |> setNothing (cell 2 3)


initWithDefaultBoard : Model
initWithDefaultBoard =
    resetBoard { isOn = defaultBoard }


init : Board -> Model
init startingBoard =
    { isOn = startingBoard }


resetBoard : Model -> Model
resetBoard model =
    update (Toggle { x = 0, y = 0 }) { isOn = defaultBoard }
        |> update (Toggle { x = 2, y = 2 })


cell : Int -> Int -> LightIndex
cell x y =
    { x = x, y = y }


setNothing : LightIndex -> Board -> Board
setNothing { x, y } =
    Matrix.set x y Nothing


isSolved : Model -> Bool
isSolved model =
    let
        isOn maybeIsOn =
            maybeIsOn
                |> Maybe.withDefault False

        onLights =
            Matrix.filter isOn model.isOn
    in
        Array.isEmpty onLights



-- UPDATE


type Msg
    = Toggle LightIndex
    | Restart


type alias LightIndex =
    { x : Int, y : Int }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle indexToggle ->
            { model | isOn = toggleLight indexToggle model.isOn }

        Restart ->
            resetBoard model


toggleLight : LightIndex -> Board -> Board
toggleLight { x, y } matrix =
    let
        toggle b =
            Maybe.map not b
    in
        matrix
            |> Matrix.update x y toggle
            |> Matrix.update (x + 1) y toggle
            |> Matrix.update (x - 1) y toggle
            |> Matrix.update x (y + 1) toggle
            |> Matrix.update x (y - 1) toggle



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "50%" )
            , ( "min-height", "400px" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ button [ onClick Restart ] [ text "Restart" ]
        , div []
            [ if isSolved model then
                showWin
              else
                gameView model
            ]
        ]


showWin : Html Msg
showWin =
    div
        [ style
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ text "You Win!" ]


gameView : Model -> Html Msg
gameView model =
    model.isOn
        |> Matrix.indexedMap lightButton
        |> matrixToDivs


matrixToDivs : Matrix (Html Msg) -> Html Msg
matrixToDivs matrix =
    let
        makeRow y =
            Matrix.getRow y matrix
                |> Maybe.map (Array.toList)
                |> Maybe.withDefault []
                |> div []

        height =
            Matrix.height matrix
    in
        [0..height]
            |> List.map makeRow
            |> div []


lightButton : Int -> Int -> Maybe Bool -> Html Msg
lightButton x y maybeIsOn =
    case maybeIsOn of
        Nothing ->
            div
                [ style
                    [ ( "width", "80px" )
                    , ( "height", "80px" )
                    , ( "border-radius", "4px" )
                    , ( "margin", "2px" )
                    , ( "display", "inline-block" )
                    ]
                ]
                []

        Just isOn ->
            div
                [ style
                    [ ( "background-color"
                      , if isOn then
                            "orange"
                        else
                            "grey"
                      )
                    , ( "width", "80px" )
                    , ( "height", "80px" )
                    , ( "border-radius", "4px" )
                    , ( "margin", "2px" )
                    , ( "display", "inline-block" )
                    ]
                , onClick (Toggle { x = x, y = y })
                ]
                []
