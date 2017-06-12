module DataFormat exposing (..)

import Models exposing (..)
import Html exposing (..)


formatInfoText : List String
formatInfoText =
    [ "Velg formatering. Mulige valg:"
    , "d : distanse      - antall passerte meter for hver passering"
    , "T : rundeTid      - rundetid for runden"
    , "D : Differanse    - differanse mellom snittrundetiden og rundetiden"
    , "H : Hastighet     - hastighet for runden i km/t"
    , "P : Passeringstid - passeringstid for runden \nStandard er dTDH (som også vil bli brukt hvis det er ugyldig input)"
    ]


createFormatInfo : Model -> Html msg
createFormatInfo model =
    let
        res =
            List.filterMap
                (\x ->
                    case charToLapInfo x of
                        Nothing ->
                            Just x

                        Just _ ->
                            Nothing
                )
                model.outputFormatString

        infoTxt =
            case res of
                [] ->
                    ""

                x :: xs ->
                    " Ugyldig symbol : " ++ String.fromChar x
    in
        text infoTxt


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


charToLapInfo : Char -> Maybe LapInfo
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


defaultOutputFormat : List Char
defaultOutputFormat =
    List.map formatInfoToChar [ LapDistance, LapTime, LapDifference, LapSplitTime ]


getOutputFormat : List Char -> List LapInfo
getOutputFormat outputStr =
    let
        res =
            List.filterMap charToLapInfo outputStr
    in
        case res of
            [] ->
                List.filterMap charToLapInfo defaultOutputFormat

            _ ->
                res
