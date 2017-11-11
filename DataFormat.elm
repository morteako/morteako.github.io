module DataFormat exposing (..)

import Models exposing (..)
import Html exposing (..)


createFormatButton : LapInfo -> { lapInfo : LapInfo, on : Bool }
createFormatButton lapInfo =
    { lapInfo = lapInfo, on = True }


changeFormatButtonOrder : Float -> Dir -> List FormatButton -> List FormatButton
changeFormatButtonOrder index dir formatButtons =
    let
        newVal fb =
            case dir of
                Up ->
                    if fb.index == 0 then
                        toFloat (List.length formatButtons)
                        --put last
                    else
                        fb.index - 1.5

                Down ->
                    if fb.index == toFloat (List.length formatButtons - 1) then
                        -1.0
                        --put first
                    else
                        fb.index + 1.5

        --0.5 == 1.5 - 1
        fbs =
            List.map
                (\fb ->
                    if fb.index == index then
                        { fb | index = newVal fb }
                    else
                        fb
                )
                formatButtons
    in
        fixFormatButtonOrder fbs


fixFormatButtonOrder : List FormatButton -> List FormatButton
fixFormatButtonOrder fbs =
    fbs
        |> List.sortWith
            (\a b ->
                case ( a.on, b.on ) of
                    ( True, True ) ->
                        compare a.index b.index

                    ( True, False ) ->
                        LT

                    ( False, True ) ->
                        GT

                    ( False, False ) ->
                        EQ
            )
        |> List.indexedMap (\i fb -> { fb | index = toFloat i })



--clickFormatButton


changeSwitchFormatButton : FormatButton -> Model -> List FormatButton
changeSwitchFormatButton formatButton model =
    fixFormatButtonOrder (switchFormatButton formatButton model)


switchFormatButton : FormatButton -> Model -> List FormatButton
switchFormatButton formatButton model =
    List.map
        (\fb ->
            (if fb.lapInfo == formatButton.lapInfo then
                { fb
                    | on = not fb.on
                }
             else
                fb
            )
        )
        model.formatButtons


formatInfoText : List String
formatInfoText =
    [ "Velg formatering :"
    , "Distanse      - antall passerte meter for hver passering"
    , "Rundetid      - rundetid for runden"
    , "Differanse    - differanse mellom snittrundetiden og rundetiden"
    , "Hastighet     - hastighet for runden i km/t"
    , "Passeringstid - passeringstid for runden"
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


lapInfoToString : LapInfo -> String
lapInfoToString lapInfo =
    case lapInfo of
        LapDistance ->
            "Distanse"

        LapDifference ->
            "Differanse"

        LapTime ->
            "Rundetid"

        LapSpeed ->
            "Hastighet"

        LapSplitTime ->
            "Passeringstid"


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
