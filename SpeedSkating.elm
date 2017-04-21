module SpeedSkating exposing (..)

import Models exposing (..)
import Regex exposing (..)
import Utils exposing (..)


calculateDiffs : List ( number, number ) -> List number
calculateDiffs secs =
    List.map (uncurryFlip (-)) secs



--Cons 0 to get first split time as first lap time


calculateLapTimes : List number -> List number
calculateLapTimes numberList =
    calculateDiffs <| combine <| 0 :: numberList


distanceToString : Distance -> String
distanceToString distance =
    -- 10 just a "big" number to get all,
    String.slice 1 10 <| toString distance ++ "m"


splitToSeconds : String -> Result String Float
splitToSeconds splitTime =
    let
        parts =
            Regex.split (AtMost 1) (regex "[.]") splitTime
    in
        case parts of
            [ mins, secsMillis ] ->
                case resultMap String.toFloat parts of
                    Err e ->
                        Err e

                    Ok ints ->
                        Ok <| dotProduct ints [ 60.0, 1.0 ]

            _ ->
                Result.Err <| "Line : " ++ splitTime ++ " | Could not split at ."


getLapTimes : Model -> Result String (List Float)
getLapTimes model =
    let
        toSecondsResult =
            resultMap splitToSeconds model.splitTimes
    in
        case toSecondsResult of
            Ok v ->
                Ok <| calculateLapTimes v

            Err e ->
                Err e


checkTimesGeneral : (a -> Bool) -> String -> List a -> Result String (List a)
checkTimesGeneral pred errorMsg times =
    let
        res =
            findIndex pred times
    in
        case res of
            Just ind ->
                Err <| errorMsg ++ toString (ind + 1)

            Nothing ->
                Ok times


validMinSec time =
    let
        firstTwoChars =
            time
                |> String.split "."
                |> List.take 2
                |> List.map (String.padLeft 2 '0')
                |> List.map (String.left 1)
    in
        List.any (\x -> String.contains x "6789") firstTwoChars


checkValidMinSec : List String -> Result String (List String)
checkValidMinSec splitTimes =
    checkTimesGeneral validMinSec "Ugyldig tid. Minutter eller sekunder er over 60. Linje : " splitTimes


checkNoNegative lapTimes =
    checkTimesGeneral ((>) 0) "Negativ rundetid for runde " lapTimes



-- Nothing : No error
-- Just number : Error at line <number>


errorCheckSplitTimes : List String -> Result String (List String)
errorCheckSplitTimes splitTimes =
    checkTimesGeneral
        (not << Regex.contains (Regex.regex "^\\d{1,2}\\.\\d{1,2}\\.\\d{1,2}$"))
        "feil input! Ikke på gyldig form : D.D.D , for linje : "
        splitTimes


getNrOfLaps : Distance -> Int
getNrOfLaps dist =
    ceiling <| (toFloat <| distanceToMeters dist) / 400.0


distanceToMeters : Distance -> Int
distanceToMeters dist =
    dist
        |> toString
        |> String.slice 1 10
        |> String.toInt
        |> Result.withDefault 0


getSplitDistances dist =
    let
        nrOfLaps =
            getNrOfLaps dist

        distanceInMeters =
            distanceToMeters dist

        res =
            untilScan ((<=) nrOfLaps) ((+) 1) (\lapNr -> (toString (distanceInMeters - lapNr * 400) ++ "m")) 0
    in
        unlines <| List.reverse res


getLapTimesAsList : Model -> List Float -> String -> List String
getLapTimesAsList model lapTimesFloats firstSplitTime =
    let
        splitDistances =
            lines <| getSplitDistances model.distanceChosen

        lapTimesStrings =
            List.map (fixDecimalLength model.rounding) lapTimesFloats

        lapTimesAndDifferences =
            map2Full
                (\x y -> x ++ " : " ++ y)
                identity
                lapTimesStrings
                (getLapTimesDifferences model lapTimesFloats)
    in
        map2Full
            (\x y -> String.padRight 6 ' ' x ++ " : " ++ y)
            identity
            splitDistances
            lapTimesAndDifferences


getAvgLapTime : List Float -> Float
getAvgLapTime lapTimesFloats =
    lapTimesFloats
        |> List.tail
        |> Maybe.withDefault []
        |> average


getLapTimesDifferences : Model -> List Float -> List String
getLapTimesDifferences model lapTimes =
    let
        avgLapTime =
            getAvgLapTime lapTimes

        allDifferences =
            List.map (differenceToString model) <|
                List.map (\x -> x - avgLapTime) lapTimes
    in
        case allDifferences of
            first :: rest ->
                "" :: rest

            _ ->
                []


updateRounding : Model -> Model
updateRounding model =
    { model | rounding = nextRounding model.rounding }


nextRounding : RoundingType -> RoundingType
nextRounding rounding =
    case rounding of
        OneDecimal ->
            TwoDecimal

        TwoDecimal ->
            OneDecimal


differenceToString : Model -> Float -> String
differenceToString model x =
    let
        number =
            fixDecimalLength model.rounding x
    in
        if String.startsWith "-" number then
            number
        else
            "+" ++ number


nextDecimalInfo dec =
    case dec of
        TwoDecimal ->
            "en desimal"

        OneDecimal ->
            "to desimaler"


fixDecimalLength : RoundingType -> Float -> String
fixDecimalLength rounding nr =
    let
        roundedNr =
            roundToDec rounding nr

        splitted =
            String.split "." roundedNr
    in
        case splitted of
            [ intNr ] ->
                intNr ++ "." ++ String.padRight (getNumberOfZeroes rounding) '0' ""

            [ secs, millis ] ->
                secs ++ "." ++ String.padRight (getNumberOfZeroes rounding) '0' millis

            _ ->
                String.concat splitted
