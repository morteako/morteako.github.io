module Css exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


unadjustable : Attribute msg
unadjustable =
    style [ ( "resize", "none" ) ]


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "0.8em" )
        , ( "text-align", "bottom" )
        ]


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


styleDistanceButton =
    style
        [ ( "background-color", "#008CBA" )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "padding", "15px 32px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "display", "inline-block" )
        , ( "font-size", "16px" )
        , ( "border-radius", "8px" )
        ]


styleCalculateButton =
    style
        [ ( "background-color", "#4CAF50" )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "padding", "15px 32px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "display", "inline-block" )
        , ( "font-size", "16px" )
        , ( "border-radius", "8px" )
        ]
