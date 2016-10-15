module Types exposing (..)

import Color exposing (Color)

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



type alias Model =
    Editor
