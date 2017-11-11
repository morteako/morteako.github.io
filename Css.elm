module Css exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


unadjustable : Attribute msg
unadjustable =
    style [ ( "resize", "none" ), ( "font-size", "16px" ) ]


grey =
    "#808080"


greylight =
    "#D3D3D3"


blue =
    "#008CBA"


green =
    "#4CAF50"


orange =
    "#FF8C00"


col =
    "#00FF80"


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "0.8em" )
        , ( "text-align", "bottom" )
        ]


createDataFormatStyle col =
    style <|
        ( "background-color", col )
            :: basicDataFormatStyle


basicDataFormatStyle =
    [ ( "border", "none" )
    , ( "color", "white" )
    , ( "padding", "15px 40px" )
    , ( "text-align", "center" )
    , ( "text-decoration", "none" )
    , ( "display", "inline-block" )
    , ( "font-size", "16px" )
    , ( "border-radius", "20%" )
    , ( "margin-bottom", "1px" )
    , ( "outline", "none" )
    ]


testButtonStyle =
    style <| ( "background-color", greylight ) :: basicStyleDistanceButton


instructionMsgStyle =
    style
        [ ( "font-family", "Pacifico" )
        , ( "font-size", "30px" )
        , ( "text-allign", "center" )
        , ( "width", "50%" )
        , ( "margin", "auto" )
        ]


errorMsgStyle =
    style
        [ ( "font-family", "Times New Roman" )
        , ( "font-size", "30px" )
        , ( "text-allign", "center" )
        , ( "color", "red" )
        ]


warningMsgStyle =
    style
        [ ( "font-family", "Times New Roman" )
        , ( "font-size", "30px" )
        , ( "text-allign", "center" )
        , ( "color", "orange" )
        ]


styleDistanceButton =
    style <|
        ( "background-color", grey )
            :: basicStyleDistanceButton


styleDistanceButtonChosen =
    style <|
        ( "background-color", blue )
            :: basicStyleDistanceButton


basicStyleDistanceButton =
    [ ( "border", "none" )
    , ( "color", "white" )
    , ( "padding", "15px 32px" )
    , ( "text-align", "center" )
    , ( "text-decoration", "none" )
    , ( "display", "inline-block" )
    , ( "font-size", "16px" )
    , ( "border-radius", "20%" )
    , ( "margin-bottom", "10px" )
    , ( "outline", "none" )
    ]


styleCalculateButton =
    style
        [ ( "background-color", green )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "padding", "15px 32px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "display", "inline-block" )
        , ( "font-size", "32px" )
        , ( "border-radius", "8px" )
        , ( "width", "680px" )
        , ( "height", "100px" )
        , ( "margin-bottom", "10px" )
        ]


styleDecimalButton =
    style
        [ ( "background-color", orange )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "padding", "15px 32px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "display", "inline-block" )
        , ( "font-size", "16px" )
        , ( "border-radius", "8px" )
        , ( "width", "226px" )
        , ( "margin-bottom", "10px" )
        ]


styleModeButton =
    style
        [ ( "background-color", "red" )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "padding", "15px 32px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "display", "inline-block" )
        , ( "font-size", "16px" )
        , ( "border-radius", "8px" )
        , ( "width", "226px" )
        , ( "margin-bottom", "10px" )
        ]


disabled =
    style
        [ ( "opacity", "0.7" )
        , ( "cursor", "not-allowed" )
        ]


styleTabSize =
    style
        [ ( "-moz-tab-size", "4" )
        , ( "tab-size", "4" )
        ]


styleNoOutline =
    style [ ( "outline", "none" ), ( "border", "none" ) ]


centered =
    style
        [--[ ( "position", "absolute" )
         --, ( "top", "0px" )
         --, ( "left", "33%" )
         -- , ( "margin-left", "-500px" )
        ]
