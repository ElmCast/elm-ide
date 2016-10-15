port module Editor exposing (init, update, view, subscriptions)

import Array exposing (Array)
import Bitwise exposing (and, shiftRight)
import Color exposing (Color, rgb)
import Collage exposing (collage)
import Dom
import Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Task
import Text exposing (Text)
import Transform


-- MODEL


type alias Cursor =
    { line : Int
    , column : Int
    }


type alias Highlight =
    { foreground : Maybe Color
    , background : Maybe Color
    , special : Maybe Color
    , bold : Maybe Bool
    , italic : Maybe Bool
    , underline : Maybe Bool
    , undercurl : Maybe Bool
    , reverse : Maybe Bool
    }


type alias Region =
    { top : Int
    , left : Int
    , right : Int
    , bottom : Int
    }


type alias Editor =
    { columns : Int
    , lines : Int
    , fgColor : Color
    , bgColor : Color
    , spColor : Color
    , focus : Bool
    , highlight : Highlight
    , cursor : Cursor
    , scroll : Int
    , scrollRegion : Region
    , mode : String
    , busy : Bool
    , mouse : Bool
    , title : String
    , icon : String
    , errors : List String
    , console : List ( String, Highlight )
    }


type alias Model =
    Editor


init : ( Model, Cmd Msg )
init =
    ( Editor
        120
        40
        Color.black
        Color.white
        Color.red
        False
        (Highlight Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        (Cursor 0 0)
        0
        (Region 0 0 0 0)
        "normal"
        False
        True
        "ElmIDE"
        ""
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


bell : Cmd msg
bell =
    send "bell" <| JE.null


setTitle : String -> Cmd msg
setTitle title =
    send "set-title" <| JE.string title


sendInput : String -> Cmd msg
sendInput input =
    send "input" <| JE.string input


focus : String -> Cmd Msg
focus id =
    Dom.focus id
        |> Task.perform (always NoOp) (always NoOp)


type Msg
    = Bell Int
    | Busy Bool
    | Clear
    | CursorGoto Int Int
    | Disconnect
    | EolClear
    | Error String
    | HighlightSet (List Highlight)
    | InputBlur
    | InputFocus
    | InputText String
    | ModeChange String
    | Mouse Bool
    | NoOp
    | Put (List String)
    | Ready
    | Resize Int Int
    | Scroll Int
    | SetIcon String
    | SetScrollRegion Int Int Int Int
    | SetTitle String
    | UpdateBg Color
    | UpdateFg Color
    | UpdateSp Color
    | VisualBell Int


splice : Int -> List a -> List a -> List a
splice index new old =
    let
        left =
            List.take index old

        right =
            List.drop (index + (List.length new)) old
    in
        left ++ new ++ right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bell rings ->
            ( model, bell )

        Busy bool ->
            ( { model | busy = bool }, Cmd.none )

        Clear ->
            ( model, Cmd.none )

        CursorGoto column line ->
            ( { model | cursor = Cursor column line }, Cmd.none )

        Disconnect ->
            ( model, Cmd.none )

        EolClear ->
            let
                index =
                    model.cursor.line * model.columns + model.cursor.column

                new =
                    List.repeat
                        (model.columns - model.cursor.column)
                        ( "", Highlight Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

                console =
                    splice index new model.console
            in
                ( { model | console = console }, Cmd.none )

        Error message ->
            ( { model | errors = message :: model.errors }, Cmd.none )

        HighlightSet highlights ->
            ( { model | highlight = Maybe.withDefault model.highlight (List.head (List.reverse highlights)) }, Cmd.none )

        InputBlur ->
            { model | focus = True } ! [ sendInput "<FocusLost>", focus "input" ]

        InputFocus ->
            ( { model | focus = False }, sendInput "<FocusGained>" )

        InputText text ->
            ( model
            , sendInput
                (if text == "<" then
                    "<LT>"
                 else
                    text
                )
            )

        ModeChange mode ->
            ( { model | mode = mode }, Cmd.none )

        Mouse bool ->
            ( { model | mouse = bool }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        Put strings ->
            let
                index =
                    model.cursor.line * model.columns + model.cursor.column

                new =
                    List.map (\string -> ( string, model.highlight )) strings

                console =
                    splice index new model.console

                cursor =
                    Cursor model.cursor.line (model.cursor.column + (List.length new))
            in
                ( { model | console = console, cursor = cursor }, Cmd.none )

        Ready ->
            model ! [ attach model.columns model.lines, focus "input" ]

        Resize columns lines ->
            let
                console =
                    (List.repeat (columns * lines) ( "", model.highlight ))
            in
                ( { model | columns = columns, lines = lines, console = console }, Cmd.none )

        Scroll scroll ->
            ( { model | scroll = scroll }, Cmd.none )

        SetScrollRegion top left right bottom ->
            ( { model | scrollRegion = Region top left right bottom }, Cmd.none )

        SetIcon icon ->
            ( { model | icon = icon }, Cmd.none )

        SetTitle title ->
            ( { model | title = title }, setTitle title )

        UpdateBg color ->
            ( { model | bgColor = color }, Cmd.none )

        UpdateFg color ->
            ( { model | fgColor = color }, Cmd.none )

        UpdateSp color ->
            ( { model | spColor = color }, Cmd.none )

        VisualBell rings ->
            ( model, Cmd.none )



-- VIEW


viewCell : Float -> Float -> Bool -> Color -> Color -> String -> Highlight -> Collage.Form
viewCell x y cursor fgColor bgColor string highlight =
    if string == "" && not cursor then
        Collage.group []
    else
        let
            fg =
                if cursor then
                    Color.white
                else
                    Maybe.withDefault fgColor highlight.foreground

            bg =
                if cursor then
                    Color.darkGray
                else
                    Maybe.withDefault bgColor highlight.background

            style =
                Text.style
                    { typeface = [ "Ubuntu Mono" ]
                    , height = Just 16
                    , color = fg
                    , bold = Maybe.withDefault False highlight.bold
                    , italic = Maybe.withDefault False highlight.italic
                    , line = Maybe.map (always Text.Under) highlight.underline
                    }

            background =
                Collage.rect 8 16
                    |> Collage.filled bg
                    |> Collage.move ( 0, -3 )

            text =
                Text.fromString string
                    |> style
                    |> Collage.text
        in
            Collage.group [ background, text ]
                |> Collage.move ( x, y )


viewConsole : Model -> Html a
viewConsole model =
    let
        width =
            toFloat (model.columns * 8)

        height =
            toFloat (model.lines * 16)

        background =
            Collage.rect (width * 2) (height * 2) |> Collage.filled model.bgColor

        cell index ( string, highlight ) =
            let
                column =
                    (index `rem` model.columns)

                line =
                    (index // model.columns)

                cursor =
                    model.cursor.column == column && model.cursor.line == line

                x =
                    toFloat (column * 8 + 4)

                y =
                    toFloat (line * -16 - 6)
            in
                viewCell x y cursor model.fgColor model.bgColor string highlight

        forms =
            background :: (List.indexedMap cell model.console)

        transform =
            Transform.translation (-width / 2) (height / 2)

        screen =
            collage
                (model.columns * 8)
                (model.lines * 16)
                [ Collage.groupTransform transform forms ]
    in
        Element.toHtml screen


view : Model -> Html Msg
view model =
    div []
        [ lazy viewConsole model
        , input
            [ style [ ( "with", "100%" ) ]
            , id "input"
            , class "hidden-input"
            , value ""
            , onFocus InputFocus
            , onInput InputText
            , onBlur InputBlur
            ]
            []
        , h1 [] [ text "Errors" ]
        , div [] <| List.map (\t -> li [] [ text t ]) model.errors
        ]



-- SUBSCRIPTIONS


port notifications : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    notifications decodeNotification


decodeColor : Int -> Color
decodeColor int =
    rgb (and (shiftRight int 16) 255) (and (shiftRight int 8) 255) (and int 255)


decodeNotification : String -> Msg
decodeNotification notification =
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
                    , redrawEolClear
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
                    , JD.fail ("Unhandled notification: " ++ notification)
                    ]
                )
                notification
    in
        case result of
            Ok message ->
                message

            Err error ->
                Error error


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


redrawUpdateBg : JD.Decoder Msg
redrawUpdateBg =
    JD.at [ "redraw", "update_bg" ] <|
        first (JD.tuple1 (UpdateBg << decodeColor) JD.int)


redrawUpdateFg : JD.Decoder Msg
redrawUpdateFg =
    JD.at [ "redraw", "update_fg" ] <|
        first (JD.tuple1 (UpdateFg << decodeColor) JD.int)


redrawUpdateSp : JD.Decoder Msg
redrawUpdateSp =
    JD.at [ "redraw", "update_sp" ] <|
        first (JD.tuple1 (UpdateSp << decodeColor) JD.int)


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
        JD.map (always EolClear) (JD.list (JD.succeed []))


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
        parseColors fg bg sp =
            Highlight
                (Maybe.map decodeColor fg)
                (Maybe.map decodeColor bg)
                (Maybe.map decodeColor sp)

        highlight =
            JD.object8 parseColors
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
        (first (first (JD.map ModeChange JD.string)))


redrawBusyStart : JD.Decoder Msg
redrawBusyStart =
    JD.at [ "redraw", "busy_start" ] <|
        first (JD.succeed (Busy True))


redrawBusyStop : JD.Decoder Msg
redrawBusyStop =
    JD.at [ "redraw", "busy_stop" ] <|
        first (JD.succeed (Busy False))


redrawMouseOn : JD.Decoder Msg
redrawMouseOn =
    JD.at [ "redraw", "mouse_on" ] <|
        JD.map (always (Mouse True)) (JD.list (JD.succeed []))


redrawMouseOff : JD.Decoder Msg
redrawMouseOff =
    JD.at [ "redraw", "mouse_off" ] <|
        JD.map (always (Mouse False)) (JD.list (JD.succeed []))


redrawBell : JD.Decoder Msg
redrawBell =
    JD.at [ "redraw", "bell" ] <|
        JD.map (\bells -> Bell (List.length bells)) (JD.list (JD.succeed 0))


redrawVisualBell : JD.Decoder Msg
redrawVisualBell =
    JD.at [ "redraw", "visual_bell" ] <|
        JD.map (\bells -> VisualBell (List.length bells)) (JD.list (JD.succeed 0))


redrawSetTitle : JD.Decoder Msg
redrawSetTitle =
    JD.at [ "redraw", "set_title" ] <|
        first (JD.map SetTitle JD.string)


redrawSetIcon : JD.Decoder Msg
redrawSetIcon =
    JD.at [ "redraw", "set_icon" ] <|
        first (JD.map SetIcon JD.string)
