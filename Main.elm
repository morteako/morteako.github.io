module Main exposing (..)

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


createModel : Mode -> Model
createModel mode =
    { textContent = ""
    , splitTimes = []
    , distanceChosen = D10000
    , infoMsg = Instruction mode.infoWhenBlank
    , lapTimes = []
    , lapTimesFloats = []
    , rounding = OneDecimal
    , distanceButtons = createDistanceButtons D10000
    , outputFormatString = defaultOutputFormat
    , delimiter = '\t'
    , decimalLimiter = Truncate
    , currentMode = mode
    , lapProgressionString = "0.0"
    }


model : Model
model =
    createModel lapTimeMode


splitTimesMode : Mode
splitTimesMode =
    { modeType = SplitTimesMode
    , infoWhenBlank = "Skriv inn passeringstider"
    , checkInput = Result.andThen checkValidMinSec << errorCheckSplitTimes
    , getLapTimes = getLapTimesSplitMode
    }


lapTimeMode : Mode
lapTimeMode =
    { modeType = LapTimeMode 0.0
    , infoWhenBlank = "Skriv inn åpningstid og en rundetid"
    , checkInput = Result.andThen checkStartAndLap << errorCheckLapTimes
    , getLapTimes = generateLapTimes
    }


updateInput : Model -> String -> Model
updateInput model newContent =
    let
        newLinesReplaced =
            newContent
                |> replaceRegexWith "[,:;-]" "."
                |> lines
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        errorCheckValue =
            model.currentMode.checkInput newLinesReplaced

        ( infoMsg, splitTimes ) =
            case ( newLinesReplaced, errorCheckValue ) of
                ( [], _ ) ->
                    ( Instruction model.currentMode.infoWhenBlank, [] )

                ( _, Ok _ ) ->
                    ( Instruction "Regn ut", newLinesReplaced )

                ( _, Err errorMsg ) ->
                    ( errorMsg, [] )
    in
        { model
            | textContent = newContent
            , infoMsg = infoMsg
            , splitTimes = splitTimes
        }


updateCalculate : Model -> Model
updateCalculate model =
    case model.splitTimes of
        [] ->
            { model
                | infoMsg =
                    ErrorMsg <|
                        "Kan ikke regne ut : "
                            ++ replaceRegexWith "Kan ikke regne ut : " "" (getInfoMsgString model.infoMsg)
            }

        _ ->
            let
                newModel =
                    { model | splitTimes = List.take (getNrOfLaps model.distanceChosen) model.splitTimes }

                newLapTimes =
                    newModel.currentMode.getLapTimes (ModelArg newModel)

                resultToInfoMsg res =
                    case res of
                        Ok v ->
                            Instruction "Regnet ut"

                        Err e ->
                            e

                ( errorMsg, xs ) =
                    case newLapTimes of
                        Err e ->
                            ( resultToInfoMsg (Err e), [] )

                        Ok v ->
                            ( resultToInfoMsg (checkNoNegative v), v )
            in
                case xs of
                    [] ->
                        { newModel
                            | lapTimes = lines <| getSplitDistances newModel.distanceChosen
                            , lapTimesFloats = []
                            , infoMsg = errorMsg
                        }

                    floatList ->
                        { newModel
                            | lapTimes =
                                getLapTimesAsList
                                    { newModel | lapTimesFloats = floatList }
                                    (Maybe.withDefault "" <| List.head newModel.splitTimes)
                            , lapTimesFloats = floatList
                            , infoMsg = errorMsg
                        }


updateDecimalLimiter : Model -> Model
updateDecimalLimiter model =
    { model
        | decimalLimiter =
            case model.decimalLimiter of
                Round ->
                    Truncate

                Truncate ->
                    Round
    }


updateLapProgessionInput : String -> Model -> Model
updateLapProgessionInput input model =
    case String.toFloat input of
        Err _ ->
            { model | currentMode = changeLapProgression model.currentMode 0.0, lapProgressionString = "" }

        Ok v ->
            { model | currentMode = changeLapProgression model.currentMode v, lapProgressionString = toString v }


changeLapProgression : Mode -> Float -> Mode
changeLapProgression mode val =
    case mode.modeType of
        LapTimeMode _ ->
            { mode | modeType = LapTimeMode val }

        SplitTimesMode ->
            mode


update : Msg -> Model -> Model
update msg model =
    case msg of
        AreaInput newContent ->
            updateInput model newContent

        FormatInput format ->
            { model | outputFormatString = String.toList format }

        DistanceButtonClicked dist ->
            update (AreaInput model.textContent)
                { model
                    | distanceChosen = dist
                    , lapTimes = []
                    , splitTimes = List.take (getNrOfLaps dist) model.splitTimes
                    , distanceButtons = createDistanceButtons dist
                }

        CalculateButtonClicked ->
            updateCalculate model

        RoundingButtonClicked ->
            update CalculateButtonClicked (updateRounding model)

        DecimalLimiterClicked ->
            update CalculateButtonClicked (updateDecimalLimiter model)

        TestDataButtonClicked testData ->
            update (AreaInput testData) model

        ModeButtonClicked ->
            changeMode model

        LapProgressionInput input ->
            updateLapProgessionInput input model


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
                ++ [ linebreak
                   , text "Velg formatering : "
                   , input [ style [ ( "width", "90px" ) ], onInput FormatInput, value <| String.fromList model.outputFormatString ] []
                   , createFormatInfo model
                   , linebreak
                   , linebreak
                   , textAsTextarea [ cols 81, rows <| 1 + List.length formatInfoText ] <| String.join "\n\t" formatInfoText
                   ]
            )


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
    [ button [ styleDistanceButton, onClick <| TestDataButtonClicked Test.testData10k ] [ text "Testdata : Morten - 10k" ]
    , button [ styleDistanceButton, onClick <| TestDataButtonClicked Test.testDataWorldRecord10k ] [ text "Testdata : Bloomen - 10k" ]
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


nextModeInfo : Mode -> String
nextModeInfo mode =
    case mode.modeType of
        SplitTimesMode ->
            "Passeringstid"

        LapTimeMode _ ->
            "Rundetid"


changeMode : Model -> Model
changeMode model =
    { model
        | currentMode =
            case model.currentMode.modeType of
                SplitTimesMode ->
                    lapTimeMode

                LapTimeMode _ ->
                    splitTimesMode
    }


nextDecimalLimiterInfo : DecimalLimiter -> String
nextDecimalLimiterInfo lim =
    case lim of
        Round ->
            "trunkering"

        Truncate ->
            "avrunding"


getInfoMsgString : InfoMsg -> String
getInfoMsgString infoMsg =
    case infoMsg of
        Instruction string ->
            string

        ErrorMsg string ->
            string

        WarningMsg string ->
            "Advarsel: " ++ string


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


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, update = update, view = view }
