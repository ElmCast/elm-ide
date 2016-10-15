port module Command exposing (attach, bell, setTitle, sendInput)

import Json.Encode as JE

port command : String -> Cmd msg

send : String -> JE.Value -> Cmd msg
send method data =
    command <|
        JE.encode 0 (JE.object [ ( "command", JE.string method ), ( "data", data ) ])


attach : Int -> Int -> Cmd msg
attach columns lines =
    send "attach" <|
        JE.object [ ( "columns", JE.int columns ), ( "lines", JE.int lines ) ]


bell : Cmd msg
bell =
    send "bell" <| JE.null


setTitle : String -> Cmd msg
setTitle title =
    send "set-title" <| JE.string title


sendInput : String -> Cmd msg
sendInput input =
    send "input" <| JE.string input
