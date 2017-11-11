module Main exposing (..)

import Models exposing (..)
import Html exposing (..)
import Utils exposing (..)
import SpeedSkating exposing (..)
import DataFormat exposing (..)
import LapTimeMode exposing (..)
import Basics exposing (max, min)
import View exposing (..)


model : Model
model =
    createModel splitTimesMode


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
    , outputFormatString = createFormatString formatButtonsList
    , delimiter = '\t'
    , decimalLimiter = Truncate
    , currentMode = mode
    , lapProgressionString = "0.0"
    , formatButtons = formatButtonsList
    }


formatButtonsList : List FormatButton
formatButtonsList =
    [ LapDistance
    , LapTime
    , LapDifference
    , LapSpeed
    ]
        |> List.map createFormatButton
        |> (\item list -> list ++ item) [ { lapInfo = LapSplitTime, on = False } ]
        |> List.indexedMap
            (\i fb -> { lapInfo = fb.lapInfo, on = fb.on, index = toFloat i })


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
            let
                newModel =
                    { model
                        | formatButtons =
                            List.map
                                (\fb ->
                                    if fb.lapInfo == LapSplitTime then
                                        { fb | on = True }
                                    else
                                        fb
                                )
                                model.formatButtons
                    }
            in
                updateInput (changeMode newModel) newModel.textContent

        LapProgressionInput input ->
            updateLapProgessionInput input model

        FormatButtonClicked formatButton ->
            updateCalculate <| updateFormatButtons (changeSwitchFormatButton formatButton model) model

        FormatButtonOrderClicked formatButton dir ->
            updateCalculate <| updateFormatButtons (changeFormatButtonOrder formatButton.index dir model.formatButtons) model


updateFormatButtons : List FormatButton -> Model -> Model
updateFormatButtons formatButtons model =
    { model | outputFormatString = createFormatString formatButtons, formatButtons = formatButtons }



--fix indexes


createFormatString : List FormatButton -> List Char
createFormatString formatButtons =
    formatButtons
        |> List.filter (\x -> x.on)
        |> List.map (\fb -> formatInfoToChar fb.lapInfo)


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


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, update = update, view = view }
