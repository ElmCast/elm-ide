port module Editor exposing (init, update, view, subscriptions)

import Array exposing (Array)
import Bitwise exposing (and, shiftRight)
import Color exposing (Color, rgb)
import Collage exposing (collage)
import Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Text exposing (Text)
import Transform


-- MODEL


type alias Cursor =
    { line : Int
    , column : Int
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


type alias Region =
    { top : Int
    , left : Int
    , right : Int
    , bottom : Int
    }


type alias Editor =
    { columns : Int
    , lines : Int
    , fgColor : Int
    , bgColor : Int
    , spColor : Int
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
        0
        0
        0
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


type Msg
    = Bell
    | Busy Bool
    | Clear
    | CursorGoto Int Int
    | Disconnect
    | EolClear
    | Error String
    | HighlightSet (List Highlight)
    | ModeChange String
    | Mouse Bool
    | Put (List String)
    | Ready
    | Resize Int Int
    | Scroll Int
    | SetIcon String
    | SetScrollRegion Int Int Int Int
    | SetTitle String
    | UpdateBg Int
    | UpdateFg Int
    | UpdateSp Int
    | VisualBell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bell ->
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
            ( model, Cmd.none )

        Error message ->
            ( { model | errors = message :: model.errors }, Cmd.none )

        HighlightSet highlights ->
            ( { model | highlight = Maybe.withDefault model.highlight (List.head (List.reverse highlights)) }, Cmd.none )

        ModeChange mode ->
            ( { model | mode = mode }, Cmd.none )

        Mouse bool ->
            ( { model | mouse = bool }, Cmd.none )

        Put strings ->
            let
                index =
                    model.cursor.line * model.columns + model.cursor.column

                x =
                    index `rem` model.columns

                y =
                    index // model.columns

                left =
                    List.take index model.console

                middle =
                    List.map (\string -> ( string, model.highlight )) strings

                right =
                    List.drop (index + (List.length middle)) model.console

                console =
                    left ++ middle ++ right

                cursor =
                    Cursor model.cursor.line (model.cursor.column + (List.length middle))
            in
                ( { model | console = console, cursor = cursor }, Cmd.none )

        Ready ->
            ( model, attach model.columns model.lines )

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

        UpdateFg int ->
            ( { model | fgColor = int }, Cmd.none )

        UpdateBg int ->
            ( { model | bgColor = int }, Cmd.none )

        UpdateSp int ->
            ( { model | spColor = int }, Cmd.none )

        VisualBell ->
            ( model, Cmd.none )



-- VIEW


intToColor : Int -> Color
intToColor int =
    rgb (and (shiftRight int 16) 255) (and (shiftRight int 8) 255) (and int 255)


viewCell : Int -> Model -> ( String, Highlight ) -> Collage.Form
viewCell i model ( string, highlight ) =
    if string == "" then
        Collage.group []
    else
        let
            x =
                (i `rem` model.columns) * 8 + 4

            y =
                (i // model.columns) * -16 - 6

            style =
                Text.style
                    { typeface = [ "Ubuntu Mono" ]
                    , height = Just 16
                    , color = (intToColor (Maybe.withDefault model.fgColor highlight.foreground))
                    , bold = Maybe.withDefault False highlight.bold
                    , italic = Maybe.withDefault False highlight.italic
                    , line = Maybe.map (always Text.Under) highlight.underline
                    }

            background =
                Collage.rect 8 16
                    |> Collage.filled (intToColor (Maybe.withDefault model.bgColor highlight.background))
                    |> Collage.move ( 0, -3 )

            text =
                Text.fromString string
                    |> style
                    |> Collage.text
        in
            if string == "" then
                Collage.group []
            else
                Collage.group [ background, text ]
                    |> Collage.move ( toFloat x, toFloat y )


viewConsole : Model -> Html a
viewConsole model =
    let
        width =
            toFloat (model.columns * 8)

        height =
            toFloat (model.lines * 16)

        background =
            Collage.rect (width * 2) (height * 2) |> Collage.filled (intToColor model.bgColor)

        forms =
            background :: (List.indexedMap (\i f -> viewCell i model f) model.console)

        transform =
            Transform.translation (-width / 2) (height / 2)

        screen =
            collage (model.columns * 8) (model.lines * 16) [ Collage.groupTransform transform forms ]
    in
        Element.toHtml screen


view : Model -> Html Msg
view model =
    div []
        [ viewConsole model
        , h1 [] [ text "Errors" ]
        , div [] <| List.map (\t -> li [] [ text t ]) model.errors
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
        first (JD.succeed (Busy True))


redrawBusyStop : JD.Decoder Msg
redrawBusyStop =
    JD.at [ "redraw", "busy_stop" ] <|
        first (JD.succeed (Busy False))


redrawMouseOn : JD.Decoder Msg
redrawMouseOn =
    JD.at [ "redraw", "mouse_on" ] <|
        first (JD.succeed (Mouse True))


redrawMouseOff : JD.Decoder Msg
redrawMouseOff =
    JD.at [ "redraw", "mouse_off" ] <|
        first (JD.succeed (Mouse False))


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
