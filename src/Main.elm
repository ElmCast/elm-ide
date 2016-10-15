module Main exposing (..)

import Html.App
import State
import View


main =
    Html.App.program
        { init = State.init
        , update = State.update
        , view = View.root
        , subscriptions = State.subscriptions
        }
