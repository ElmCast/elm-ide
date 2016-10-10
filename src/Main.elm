module Main exposing (..)

import Html.App
import Editor


main =
    Html.App.program
        { init = Editor.init
        , update = Editor.update
        , view = Editor.view
        , subscriptions = Editor.subscriptions
        }
