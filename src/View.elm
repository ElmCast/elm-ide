module View exposing (root)

import Collage exposing (collage)
import Color exposing (Color)
import Element exposing (Element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Text exposing (Text)
import Transform
import Types exposing (..)

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


root : Model -> Html Msg
root model =
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
