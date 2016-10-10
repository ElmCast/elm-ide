port module Editor exposing (init, update, view, subscriptions)

import Html exposing (..)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import ParseInt
import String


-- MODEL


type alias Cursor =
    { line : Int
    , column : Int
    }


type alias Font =
    { fg : String
    , bg : String
    , sp : String
    , bold : Bool
    , italic : Bool
    , underline : Bool
    , undercurl : Bool
    , drawWidth : Float
    , drawHeight : Float
    , width : Float
    , height : Float
    , size : Int
    , face : String
    }


type alias Highlight =
    { foreground : Maybe Int
    , background : Maybe Int
    , special : Maybe Int
    , bold : Maybe Bool
    , italic : Maybe Bool
    , underline : Maybe Bool
    , undercurl : Maybe Bool
    , reverse : Maybe Bool
    }


type alias ScreenDrag =
    { line : Int
    , column : Int
    }


type alias ScreenScroll =
    { x : Float
    , y : Float
    }


type alias Region =
    { top : Float
    , left : Float
    , right : Float
    , bottom : Float
    }


type alias Editor =
    { columns : Int
    , lines : Int
    , width : Float
    , height : Float
    , font : Font
    , fgColor : String
    , bgColor : String
    , spColor : String
    , cursor : Cursor
    , mode : String
    , busy : Bool
    , mouseEnabled : Bool
    , screenDragging : ScreenDrag
    , title : String
    , icon : String
    , screenScrolling : ScreenScroll
    , scrollRegion : Region
    , focused : Bool
    , lineHeight : Float
    , cursorDelay : Float
    , cursorBlink : Bool
    , cursorFloaterval : Int
    , messages : List String
    , errors : List String
    }


type alias Model =
    Editor


init : ( Model, Cmd Msg )
init =
    ( Editor
        80
        25
        0
        0
        (Font "#ffffff" "000000" "" False False False False 1 1 1 1 16 "monospace")
        ""
        ""
        ""
        (Cursor 0 0)
        "normal"
        False
        True
        (ScreenDrag 0 0)
        "elm neovim"
        ""
        (ScreenScroll 0 0)
        (Region 0 0 0 0)
        True
        1.2
        10
        False
        1000
        []
        []
    , Cmd.none
    )



-- UPDATE


port command : String -> Cmd msg


send : String -> JE.Value -> Cmd msg
send method data =
    command <|
        JE.encode 0 (JE.object [ ( "command", JE.string method ), ( "data", data ) ])


attach : Int -> Int -> Cmd msg
attach columns lines =
    send "attach" <|
        JE.object [ ( "columns", JE.int columns ), ( "lines", JE.int lines ) ]


type Msg
    = Bell
    | BusyStart
    | BusyStop
    | ChangeCursorDrawDelay
    | Clear
    | EolClear
    | CursorGoto Int Int
    | DragEnd
    | DragStart
    | DragUpdate
    | EditorMessage String
    | MouseOn
    | MouseOff
    | HighlightSet (List Highlight)
    | Input
    | ModeChange String
    | Ready
    | Disconnect
    | Error String
    | Put (List String)
    | Resize Int Int
    | Scroll Int
    | SetIcon String
    | SetScrollRegion Int Int Int Int
    | SetTitle String
    | StartBlinkCursor
    | StopBlinkCursor
    | UpdateBg Int
    | UpdateFg Int
    | UpdateSp Int
    | UpdateFontFace
    | UpdateFontPx
    | UpdateFontSize
    | UpdateLineHeight
    | UpdateScreenBounds
    | UpdateScreenSize
    | VisualBell
    | WheelScroll
    | FocusChanged


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Error message ->
            ( { model | errors = message :: model.errors }, Cmd.none )

        Ready ->
            ( model, attach model.columns model.lines )

        CursorGoto column line ->
            ( { model | cursor = Cursor column line }, Cmd.none )

        UpdateFg int ->
            ( { model | fgColor = "#" ++ (ParseInt.toHex int) }, Cmd.none )

        UpdateBg int ->
            ( { model | bgColor = "#" ++ (ParseInt.toHex int) }, Cmd.none )

        UpdateSp int ->
            ( { model | spColor = "#" ++ (ParseInt.toHex int) }, Cmd.none )

        Put strings ->
            let
                string =
                    String.join "" strings
            in
                ( model, Cmd.none )

        Resize columns lines ->
            ( { model | columns = columns, lines = lines }, Cmd.none )

        SetTitle title ->
            ( { model | title = title }, Cmd.none )

        SetIcon icon ->
            ( { model | icon = icon }, Cmd.none )

        EditorMessage message ->
            ( { model | messages = model.messages ++ [ message ] }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Errors" ]
        , div [] <| List.map (\t -> li [] [ text t ]) model.errors
        , h1 [] [ text "Messages" ]
        , div [] <| List.map (\t -> li [] [ text t ]) model.messages
        ]



-- SUBSCRIPTIONS


port notifications : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    notifications parseNotification


parseNotification : String -> Msg
parseNotification notification =
    let
        result =
            JD.decodeString
                (JD.oneOf
                    [ systemDisconnect
                    , systemError
                    , systemReady
                    , redrawUpdateFg
                    , redrawUpdateBg
                    , redrawUpdateSp
                    , redrawResize
                    , redrawClear
                    , redrawCursorGoto
                    , redrawPut
                    , redrawHighlightSet
                    , redrawScroll
                    , redrawSetScrollRegion
                    , redrawModeChange
                    , redrawBusyStart
                    , redrawBusyStop
                    , redrawMouseOn
                    , redrawMouseOff
                    , redrawBell
                    , redrawVisualBell
                    , redrawSetTitle
                    , redrawSetIcon
                    , JD.fail "Unhandled notification."
                    ]
                )
                notification
    in
        Result.withDefault (EditorMessage notification) result


first : JD.Decoder a -> JD.Decoder a
first decoder =
    JD.tuple1 identity decoder


systemError : JD.Decoder Msg
systemError =
    JD.at [ "system", "error" ] <|
        JD.map Error JD.string


systemDisconnect : JD.Decoder Msg
systemDisconnect =
    JD.at [ "system", "disconnect" ] <|
        JD.succeed Disconnect


systemReady : JD.Decoder Msg
systemReady =
    JD.at [ "system", "ready" ] <|
        JD.succeed Ready


redrawUpdateFg : JD.Decoder Msg
redrawUpdateFg =
    JD.at [ "redraw", "update_fg" ] <|
        first (JD.tuple1 UpdateFg JD.int)


redrawUpdateBg : JD.Decoder Msg
redrawUpdateBg =
    JD.at [ "redraw", "update_bg" ] <|
        first (JD.tuple1 UpdateBg JD.int)


redrawUpdateSp : JD.Decoder Msg
redrawUpdateSp =
    JD.at [ "redraw", "update_sp" ] <|
        first (JD.tuple1 UpdateSp JD.int)


redrawResize : JD.Decoder Msg
redrawResize =
    JD.at [ "redraw", "resize" ] <|
        first (JD.tuple2 Resize JD.int JD.int)


redrawClear : JD.Decoder Msg
redrawClear =
    JD.at [ "redraw", "clear" ] <|
        first (JD.succeed Clear)


redrawEolClear : JD.Decoder Msg
redrawEolClear =
    JD.at [ "redraw", "eol_clear" ] <|
        first (JD.succeed EolClear)


redrawCursorGoto : JD.Decoder Msg
redrawCursorGoto =
    JD.at [ "redraw", "cursor_goto" ] <|
        first (JD.tuple2 CursorGoto JD.int JD.int)


redrawPut : JD.Decoder Msg
redrawPut =
    JD.at [ "redraw", "put" ] <|
        JD.map Put (JD.list (first JD.string))


redrawHighlightSet : JD.Decoder Msg
redrawHighlightSet =
    let
        highlight =
            JD.object8 Highlight
                (JD.maybe ("foreground" := JD.int))
                (JD.maybe ("background" := JD.int))
                (JD.maybe ("special" := JD.int))
                (JD.maybe ("bold" := JD.bool))
                (JD.maybe ("italic" := JD.bool))
                (JD.maybe ("underline" := JD.bool))
                (JD.maybe ("undercurl" := JD.bool))
                (JD.maybe ("reverse" := JD.bool))
    in
        JD.at [ "redraw", "highlight_set" ] <|
            JD.map HighlightSet (JD.list (first highlight))


redrawScroll : JD.Decoder Msg
redrawScroll =
    JD.at [ "redraw", "scroll" ] <|
        first (JD.tuple1 Scroll JD.int)


redrawSetScrollRegion : JD.Decoder Msg
redrawSetScrollRegion =
    JD.at [ "redraw", "set_scroll_region" ] <|
        first (JD.tuple4 SetScrollRegion JD.int JD.int JD.int JD.int)


redrawModeChange : JD.Decoder Msg
redrawModeChange =
    JD.at [ "redraw", "mode_change" ] <|
        first (JD.map ModeChange JD.string)


redrawBusyStart : JD.Decoder Msg
redrawBusyStart =
    JD.at [ "redraw", "busy_start" ] <|
        first (JD.succeed BusyStart)


redrawBusyStop : JD.Decoder Msg
redrawBusyStop =
    JD.at [ "redraw", "busy_stop" ] <|
        first (JD.succeed BusyStop)


redrawMouseOn : JD.Decoder Msg
redrawMouseOn =
    JD.at [ "redraw", "mouse_on" ] <|
        first (JD.succeed MouseOn)


redrawMouseOff : JD.Decoder Msg
redrawMouseOff =
    JD.at [ "redraw", "mouse_off" ] <|
        first (JD.succeed MouseOff)


redrawBell : JD.Decoder Msg
redrawBell =
    JD.at [ "redraw", "bell" ] <|
        first (JD.succeed Bell)


redrawVisualBell : JD.Decoder Msg
redrawVisualBell =
    JD.at [ "redraw", "bell" ] <|
        first (JD.succeed VisualBell)


redrawSetTitle : JD.Decoder Msg
redrawSetTitle =
    JD.at [ "redraw", "set_title" ] <|
        first (JD.map SetTitle JD.string)


redrawSetIcon : JD.Decoder Msg
redrawSetIcon =
    JD.at [ "redraw", "set_icon" ] <|
        first (JD.map SetIcon JD.string)
