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


getLapTimes : Model -> Result InfoMsg (List Float)
getLapTimes model =
    let
        toSecondsResult =
            resultMap splitToSeconds model.splitTimes
    in
        case toSecondsResult of
            Ok v ->
                Ok <| calculateLapTimes v

            Err e ->
                Err (ErrorMsg e)


checkTimesGeneral : (a -> Bool) -> (String -> InfoMsg) -> String -> List a -> Result InfoMsg (List a)
checkTimesGeneral pred infoCreator errorMsg times =
    let
        res =
            findIndex pred times
    in
        case res of
            Just ind ->
                Err <| infoCreator <| errorMsg ++ toString (ind + 1)

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


checkValidMinSec : List String -> Result InfoMsg (List String)
checkValidMinSec splitTimes =
    checkTimesGeneral validMinSec ErrorMsg "Ugyldig tid. Minutter eller sekunder er over 60. Linje : " splitTimes


checkNoNegative lapTimes =
    checkTimesGeneral ((>) 0) WarningMsg "Negativ rundetid for runde " lapTimes



-- Nothing : No error
-- Just number : Error at line <number>


errorCheckSplitTimes : List String -> Result InfoMsg (List String)
errorCheckSplitTimes splitTimes =
    checkTimesGeneral
        (not << Regex.contains (Regex.regex "^\\d{1,2}\\.\\d{1,2}\\.\\d{1,2}$"))
        ErrorMsg
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


getLapSpeeds : Model -> List String
getLapSpeeds model =
    model.lapTimesFloats
        |> List.indexedMap
            (\index x ->
                (if index > 0 then
                    400
                 else
                    firstRoundLength model.distanceChosen
                )
                    / 1000
                    / (x / 3600.0)
            )
        |> List.map (fixDecimalLength model.decimalLimiter model.rounding)
        |> List.map (\x -> x ++ " km/t")


firstRoundLength : Distance -> number
firstRoundLength dist =
    case dist of
        D500 ->
            100

        D1000 ->
            200

        D1500 ->
            300

        D3000 ->
            200

        D5000 ->
            200

        D10000 ->
            400


formatInfoToChar : LapInfo -> Char
formatInfoToChar info =
    case info of
        LapDistance ->
            'd'

        LapDifference ->
            'D'

        LapTime ->
            'T'

        LapSpeed ->
            'H'

        LapSplitTime ->
            'P'


charToLapInfo x =
    case x of
        'D' ->
            Just LapDifference

        'T' ->
            Just LapTime

        'H' ->
            Just LapSpeed

        'd' ->
            Just LapDistance

        'P' ->
            Just LapSplitTime

        _ ->
            Nothing


getLapTimesAsList : Model -> String -> List String
getLapTimesAsList model firstSplitTime =
    let
        lapInfoFunc : LapInfo -> List String
        lapInfoFunc lapInfo =
            case lapInfo of
                LapDistance ->
                    lines <| getSplitDistances model.distanceChosen

                LapDifference ->
                    (getLapTimesDifferences model)

                LapTime ->
                    List.map (fixDecimalLength model.decimalLimiter model.rounding) model.lapTimesFloats

                LapSpeed ->
                    getLapSpeeds model

                LapSplitTime ->
                    model.splitTimes

        splitDistances =
            lines <| getSplitDistances model.distanceChosen

        lapTimesStrings =
            List.map (fixDecimalLength model.decimalLimiter model.rounding) model.lapTimesFloats

        concatFullWithComma =
            map2FullId (\x y -> x ++ String.fromList [ ' ', model.delimiter, ' ' ] ++ y)

        lapInfoLists =
            List.map lapInfoFunc
                (List.filterMap charToLapInfo model.outputFormatString)
                |> \prev -> List.map (listPadRight (getNrOfLaps model.distanceChosen) "") prev

        pad ss =
            let
                -- default doesn't really matter
                maxStrLength =
                    Maybe.withDefault 1 <| List.maximum <| List.map String.length ss
            in
                List.map (String.padRight maxStrLength ' ') ss
    in
        lapInfoLists
            |> List.map pad
            |> List.foldr concatFullWithComma []


getAvgLapTime : List Float -> Float
getAvgLapTime lapTimesFloats =
    lapTimesFloats
        |> List.tail
        |> Maybe.withDefault []
        |> average


getLapTimesDifferences : Model -> List String
getLapTimesDifferences model =
    let
        avgLapTime =
            getAvgLapTime model.lapTimesFloats

        allDifferences =
            List.map (differenceToString model) <|
                List.map (\x -> x - avgLapTime) model.lapTimesFloats
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
        ZeroDecimal ->
            OneDecimal

        OneDecimal ->
            TwoDecimal

        TwoDecimal ->
            ZeroDecimal


differenceToString : Model -> Float -> String
differenceToString model x =
    let
        number =
            fixDecimalLength model.decimalLimiter model.rounding x
    in
        if String.startsWith "-" number then
            number
        else
            "+" ++ number


nextDecimalInfo dec =
    case dec of
        ZeroDecimal ->
            "en desimal"

        OneDecimal ->
            "to desimaler"

        TwoDecimal ->
            "null desimaler"


fixDecimalLength : DecimalLimiter -> RoundingType -> Float -> String
fixDecimalLength lim rounding nr =
    let
        roundedNr =
            roundToDec lim rounding nr

        splitted =
            String.split "." roundedNr

        val =
            case splitted of
                [ intNr ] ->
                    intNr ++ "." ++ String.padRight (getNumberOfZeroes rounding) '0' ""

                [ secs, millis ] ->
                    secs ++ "." ++ String.padRight (getNumberOfZeroes rounding) '0' millis

                _ ->
                    String.concat splitted
    in
        case rounding of
            ZeroDecimal ->
                roundedNr

            _ ->
                val
