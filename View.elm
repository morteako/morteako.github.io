module View exposing (..)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)
import SpeedSkating exposing (..)
import Test exposing (..)
import Css exposing (..)
import DataFormat exposing (..)
import LapTimeMode exposing (..)
import Basics exposing (max, min)


getInfoMsgCss : InfoMsg -> Attribute msg
getInfoMsgCss infoMsg =
    case infoMsg of
        Instruction _ ->
            instructionMsgStyle

        ErrorMsg _ ->
            errorMsgStyle

        WarningMsg _ ->
            warningMsgStyle


textAsTextarea : List (Attribute msg) -> String -> Html msg
textAsTextarea attributes str =
    unadjustableTextarea ([ Css.styleNoOutline, readonly True, value str ] ++ attributes) []


unadjustableTextarea : List (Attribute msg) -> List (Html msg) -> Html msg
unadjustableTextarea attributes htmls =
    textarea (Css.unadjustable :: attributes) htmls


createDistanceButtons : Distance -> List (Html Msg)
createDistanceButtons currentDist =
    let
        distances : List Distance
        distances =
            [ D500, D1000, D1500, D3000, D5000, D10000 ]

        distanceStrings =
            List.map distanceToString distances
    in
        List.map
            (\dist ->
                button
                    [ if dist == currentDist then
                        styleDistanceButtonChosen
                      else
                        styleDistanceButton
                    , onClick <| DistanceButtonClicked dist
                    ]
                    [ text <| distanceToString dist ]
            )
            distances


createLapTimeTexts : Model -> List (Html msg)
createLapTimeTexts model =
    let
        strings =
            [ "Snitt  : ", "Laveste: ", "Høyeste: " ]

        ( avg, best, worst ) =
            case model.lapTimesFloats of
                [] ->
                    ( "", "", "" )

                [ first ] ->
                    ( "", "", "" )

                --Skip first laptime because of start time ++
                first :: rest ->
                    tupleMap3
                        (fixDecimalLength model.decimalLimiter model.rounding << \f -> f rest)
                        ( average, maybeFuncWithDefault List.minimum 0.0, maybeFuncWithDefault List.maximum 0.0 )

        texts =
            List.map2 (++) strings [ avg, best, worst ]
    in
        [ textAsTextarea [ rows 3 ] <| unlines texts ]


linebreak : Html msg
linebreak =
    br [] []


viewFormatButton : FormatButton -> List (Html Msg)
viewFormatButton formatButton =
    let
        col =
            if formatButton.on then
                green
            else
                greylight
    in
        button [ Css.createDataFormatStyle col, onClick <| FormatButtonClicked formatButton ]
            [ text <|
                lapInfoToString formatButton.lapInfo
            ]
            :: button [ Css.createDataFormatStyle col, onClick <| FormatButtonOrderClicked formatButton Up ]
                [ text <| "+"
                ]
            :: button [ Css.createDataFormatStyle col, onClick <| FormatButtonOrderClicked formatButton Down ]
                [ text <| "-"
                ]
            :: [ linebreak ]


view : Model -> Html Msg
view model =
    let
        nrOfLaps =
            getNrOfLaps model.distanceChosen

        lapText =
            case model.lapTimes of
                [] ->
                    --output is just the split distances
                    getSplitDistances model.distanceChosen

                _ ->
                    unlines model.lapTimes

        inputAreaValue =
            model.textContent
                |> lines
                |> List.take (getNrOfLaps model.distanceChosen)
                |> unlines

        nrOfCols =
            Basics.max 40 <| Maybe.withDefault 40 <| List.maximum <| List.map String.length <| lines lapText
    in
        div [ centered ] <|
            ([]
                ++ [ linebreak ]
                ++ model.distanceButtons
                ++ [ linebreak ]
                ++ createCalculateButtons model
                ++ [ linebreak ]
                ++ testDataButtons
                ++ [ linebreak
                   , unadjustableTextarea [ cols 3, rows nrOfLaps, placeholder <| unlines (List.map toString (List.range 1 nrOfLaps)) ] []
                   , unadjustableTextarea [ cols 15, rows nrOfLaps, onInput AreaInput, value inputAreaValue, placeholder "Tider her...." ] []
                   , unadjustableTextarea [ cols nrOfCols, rows nrOfLaps, readonly True, value lapText, styleTabSize ] []
                   , linebreak
                   ]
                ++ createLapProgressionInput model
                ++ createLapTimeTexts model
                ++ [ linebreak ]
                ++ List.concat (List.map viewFormatButton model.formatButtons)
                ++ [ -- linebreak
                     --, text "Velg formatering : "
                     --, input [ style [ ( "width", "90px" ) ], onInput FormatInput, value <| String.fromList model.outputFormatString ] []
                     createFormatInfo model
                   , linebreak
                   , linebreak
                   , textAsTextarea [ cols 81, rows <| 1 + List.length formatInfoText ] <| String.join "\n\t" formatInfoText
                   ]
            )


createLapProgressionInput : Model -> List (Html Msg)
createLapProgressionInput model =
    case model.currentMode.modeType of
        LapTimeMode val ->
            [ linebreak
            , text "Skriv inn rundeprogresjon: "
            , input [ style [ ( "width", "90px" ) ], onInput LapProgressionInput ] []
            , createLapProgressionInfo model
            , linebreak
            ]

        _ ->
            []


testDataButtons : List (Html Msg)
testDataButtons =
    [ button [ testButtonStyle, onClick <| TestDataButtonClicked Test.testData10k ] [ text "Testdata : Morten - 10k" ]
    , button [ testButtonStyle, onClick <| TestDataButtonClicked Test.testDataWorldRecord10k ] [ text "Testdata : Bloomen - 10k" ]
    , button [ testButtonStyle, onClick <| TestDataButtonClicked Test.testHeatherBergsma ] [ text "Testdata : R. Bergsma - 1.5k" ]
    ]


createCalculateButtons : Model -> List (Html Msg)
createCalculateButtons model =
    let
        extraAttribues =
            case model.splitTimes of
                [] ->
                    [ Css.disabled, Html.Attributes.disabled True ]

                _ ->
                    []
    in
        [ button (extraAttribues ++ [ styleCalculateButton, onClick CalculateButtonClicked ]) [ text <| getInfoMsgString model.infoMsg ]
        , br [] []
        , button (extraAttribues ++ [ styleDecimalButton, onClick RoundingButtonClicked ]) [ text <| "Bytt til " ++ nextDecimalInfo model.rounding ]
        , button (extraAttribues ++ [ styleDecimalButton, onClick DecimalLimiterClicked ]) [ text <| "Bytt til " ++ nextDecimalLimiterInfo model.decimalLimiter ]
        , button [ styleModeButton, onClick ModeButtonClicked ] [ text <| "Modus: " ++ nextModeInfo model.currentMode ]
        ]


getInfoMsgString : InfoMsg -> String
getInfoMsgString infoMsg =
    case infoMsg of
        Instruction string ->
            string

        ErrorMsg string ->
            string

        WarningMsg string ->
            "Advarsel: " ++ string


nextDecimalLimiterInfo : DecimalLimiter -> String
nextDecimalLimiterInfo lim =
    case lim of
        Round ->
            "trunkering"

        Truncate ->
            "avrunding"


nextModeInfo : Mode -> String
nextModeInfo mode =
    case mode.modeType of
        SplitTimesMode ->
            "Passeringstid"

        LapTimeMode _ ->
            "Rundetid"
