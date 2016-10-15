module State exposing (init, update, subscriptions)

import Array exposing (Array)
import Color exposing (Color)
import Command exposing (..)
import Dom
import Notification
import Task
import Types exposing (..)


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

                left =
                    List.take index model.console

                middle =
                    List.repeat
                        (model.columns - model.cursor.column)
                        ( "", Highlight Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing )

                right =
                    List.drop (index + (List.length middle)) model.console

                console =
                    left ++ middle ++ right
            in
                ( { model | console = console }, Cmd.none )

        Error message ->
            ( { model | errors = message :: model.errors }, Cmd.none )

        HighlightSet highlights ->
            ( { model | highlight = Maybe.withDefault model.highlight (List.head (List.reverse highlights)) }, Cmd.none )

        InputBlur ->
            { model | focus = True } ! [ sendInput "<FocusLost>", Dom.focus "input" |> Task.perform (always NoOp) (always NoOp) ]

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
            model ! [ attach model.columns model.lines, Dom.focus "input" |> Task.perform (always NoOp) (always NoOp) ]

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Notification.sub
