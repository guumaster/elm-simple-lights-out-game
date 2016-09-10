module Main exposing (..)

import Html.App as Html
import LightsGame


-- APP


main : Program Never
main =
    Html.beginnerProgram
        { model = LightsGame.initWithDefaultBoard
        , view = LightsGame.view
        , update = LightsGame.update
        }



-- CSS STYLES
