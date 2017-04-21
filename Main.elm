module Main exposing (..)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)
import SpeedSkating exposing (..)
import Test exposing (..)
import Css exposing (..)


model : Model
model =
    { textContent = ""
    , splitTimes = []
    , distanceChosen = D10000
    , infoMsg = Instruction "Skriv inn rundetider"
    , lapTimes = []
    , lapTimesFloats = []
    , rounding = OneDecimal
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input newContent ->
            let
                newLinesReplaced =
                    newContent
                        |> replaceRegexWith "[,:]" "."
                        |> replaceRegexWith "[ ]" "\n"
                        |> lines
                        |> List.filter (not << String.isEmpty)

                errorCheckValue =
                    newLinesReplaced
                        |> errorCheckSplitTimes
                        |> Result.andThen checkValidMinSec

                ( infoMsg, splitTimes ) =
                    case ( newLinesReplaced, errorCheckValue ) of
                        ( [], _ ) ->
                            ( Instruction "Skriv inn rundetider", [] )

                        ( _, Ok _ ) ->
                            ( Instruction "OK", newLinesReplaced )

                        ( _, Err msg ) ->
                            ( ErrorMsg msg, [] )
            in
                { model
                    | textContent = newContent
                    , infoMsg = infoMsg
                    , splitTimes = splitTimes
                }

        DistanceButtonClicked dist ->
            { model | distanceChosen = dist, lapTimes = [] }

        CalculateButtonClicked ->
            case model.splitTimes of
                [] ->
                    { model | infoMsg = ErrorMsg <| "Kan ikke regne ut : " ++ replaceRegexWith "Kan ikke regne ut : " "" (getInfoMsgString model.infoMsg) }

                _ ->
                    let
                        res =
                            model
                                |> getLapTimes
                                |> Result.andThen checkNoNegative

                        ( lapTimesString, lapTimesFloats ) =
                            case res of
                                Ok floatList ->
                                    ( getLapTimesAsList
                                        model
                                        floatList
                                      <|
                                        Maybe.withDefault "" (List.head model.splitTimes)
                                    , floatList
                                    )

                                Err string ->
                                    ( (lines <| getSplitDistances model.distanceChosen), [] )
                    in
                        case res of
                            Err e ->
                                { model | lapTimes = lapTimesString, infoMsg = ErrorMsg <| "Feil i rundetidene - " ++ e }

                            Ok v ->
                                { model
                                    | lapTimes = lapTimesString
                                    , lapTimesFloats = lapTimesFloats
                                    , infoMsg = Instruction "REGNET UT"
                                }

        RoundingButtonClicked ->
            update CalculateButtonClicked (updateRounding model)



-- update


distanceButtons : List (Html Msg)
distanceButtons =
    let
        distances : List Distance
        distances =
            [ D500, D1000, D1500, D3000, D5000, D10000 ]

        distanceStrings =
            List.map distanceToString distances
    in
        List.map
            (\dist ->
                button [ styleDistanceButton, onClick <| DistanceButtonClicked dist ] [ text <| distanceToString dist ]
            )
            distances


createLapTimeTexts : Model -> List (Html msg)
createLapTimeTexts model =
    let
        strTuple =
            ( "snittrundetid: ", "beste rundetid: ", "dårligste rundetid: " )

        ( avg, best, worst ) =
            case model.lapTimesFloats of
                [] ->
                    ( "", "", "" )

                [ first ] ->
                    ( "", "", "" )

                --Skip first laptime because of start time ++
                first :: rest ->
                    tupleMap3
                        (fixDecimalLength model.rounding << \f -> f rest)
                        ( getAvgLapTime, maybeFuncWithDefault List.minimum 0.0, maybeFuncWithDefault List.maximum 0.0 )

        texts =
            List.map2 (++) (tuple3ToList strTuple) (tuple3ToList ( avg, best, worst ))
    in
        List.map (\x -> div [] [ text x ]) texts


view : Model -> Html Msg
view model =
    let
        nrOfLaps =
            getNrOfLaps model.distanceChosen

        lapText =
            case model.lapTimes of
                [] ->
                    getSplitDistances model.distanceChosen

                _ ->
                    unlines model.lapTimes

        avgLapTime =
            getAvgLapTime model.lapTimesFloats

        avgLapTimeString =
            if isNaN avgLapTime then
                ""
            else
                fixDecimalLength model.rounding avgLapTime

        inputAreaValue =
            model.textContent
                |> lines
                |> List.take (getNrOfLaps model.distanceChosen)
                |> unlines
    in
        div [] <|
            ([ div [ getInfoMsgCss model.infoMsg ] [ text <| getInfoMsgString model.infoMsg ]
             , div [] [ text <| "Distanse valgt : " ++ distanceToString model.distanceChosen ]
             , unadjustableTextarea [ cols 3, rows nrOfLaps, placeholder <| unlines (List.map toString (List.range 1 nrOfLaps)) ] []
             , unadjustableTextarea [ cols 15, rows nrOfLaps, onInput Input, value inputAreaValue ] []
             , unadjustableTextarea [ cols 30, rows nrOfLaps, readonly True, value lapText ] []
             , unadjustableTextarea [ cols 30, rows 25, readonly True, value testData10k ] []
             ]
                ++ createLapTimeTexts model
                ++ [ button [ styleCalculateButton, onClick CalculateButtonClicked ] [ text "Regn ut" ]
                   , button [ styleCalculateButton, onClick RoundingButtonClicked ] [ text <| "Bytt til " ++ nextDecimalInfo model.rounding ]
                   ]
                ++ distanceButtons
            )


getInfoMsgString infoMsg =
    case infoMsg of
        Instruction x ->
            x

        ErrorMsg x ->
            x


getInfoMsgCss infoMsg =
    case infoMsg of
        Instruction _ ->
            instructionMsgStyle

        ErrorMsg _ ->
            errorMsgStyle


unadjustableTextarea : List (Attribute msg) -> List (Html msg) -> Html msg
unadjustableTextarea attributes htmls =
    textarea (Css.unadjustable :: attributes) htmls


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, update = update, view = view }
