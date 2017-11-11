module LapTimeMode exposing (..)

import Models exposing (..)
import Html exposing (..)
import SpeedSkating exposing (..)
import Utils exposing (..)


createLapProgressionInfo : Model -> Html msg
createLapProgressionInfo model =
    let
        infoTxt =
            case String.toFloat model.lapProgressionString of
                Err e ->
                    " Ugyldig rundeprogresjons-input. Riktig format : ss.hh"

                Ok v ->
                    ""
    in
        text infoTxt


fixInterpolate splitTimes =
    List.map3 (,,) splitTimes (List.drop 1 splitTimes) (List.drop 2 splitTimes)



--|> List.map (\tup ->
--    case tup of  (a,"-",c) ->


getLapTimesSplitMode : ModelArg -> Result InfoMsg (List Float)
getLapTimesSplitMode (ModelArg model) =
    let
        interpolated =
            (,)

        toSecondsResult =
            resultMap splitToSeconds model.splitTimes
    in
        case toSecondsResult of
            Ok v ->
                Ok <| calculateLapTimes v

            Err e ->
                Err (ErrorMsg e)


inputToStartAndLap : List Float -> Result String ( Float, Float )
inputToStartAndLap splitTimes =
    case splitTimes of
        [ start, lap ] ->
            Ok <| ( start, lap )

        _ ->
            Err "Feil: ikke skrevet inn en starttid og en rundetid"


generateLapTimes : ModelArg -> Result InfoMsg (List Float)
generateLapTimes (ModelArg model) =
    case resultMap String.toFloat model.splitTimes of
        Err e ->
            Err <| ErrorMsg e

        Ok v ->
            case ( List.head v, last v ) of
                ( Just start, Just lapTime ) ->
                    let
                        lapProg =
                            case model.currentMode.modeType of
                                LapTimeMode lapVal ->
                                    lapVal

                                _ ->
                                    0.0

                        res =
                            lapTime
                                |> List.repeat (getNrOfLaps model.distanceChosen - List.length v)
                                |> List.indexedMap (\i a -> lapProg * (toFloat i + 1) + lapTime)
                    in
                        Ok
                            (v ++ res)

                _ ->
                    Err <| ErrorMsg "Ikke skrevet inn startid og rundetid"
