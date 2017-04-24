module Main exposing (..)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Utils exposing (..)
import SpeedSkating exposing (..)
import Test exposing (..)
import Css exposing (..)


defaultOutputFormat : List Char
defaultOutputFormat =
    List.map formatInfoToChar [ LapDistance, LapTime, LapDifference, LapSpeed ]


model : Model
model =
    { textContent = ""
    , splitTimes = []
    , distanceChosen = D10000
    , infoMsg = Instruction "Skriv inn rundetider"
    , lapTimes = []
    , lapTimesFloats = []
    , rounding = OneDecimal
    , distanceButtons = createDistanceButtons D10000
    , outputFormatString = defaultOutputFormat
    , delimiter = '\t'
    , decimalLimiter = Truncate
    }


updateInput : Model -> String -> Model
updateInput model newContent =
    let
        newLinesReplaced =
            newContent
                |> replaceRegexWith "[,:;]" "."
                |> replaceRegexWith "[ ]" "\n"
                |> lines
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)

        errorCheckValue =
            newLinesReplaced
                |> errorCheckSplitTimes
                |> Result.andThen checkValidMinSec

        ( infoMsg, splitTimes ) =
            case ( newLinesReplaced, errorCheckValue ) of
                ( [], _ ) ->
                    ( Instruction "Skriv inn passeringstider", [] )

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
                    getLapTimes newModel

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
                case ( errorMsg, xs ) of
                    ( _, [] ) ->
                        { newModel
                            | lapTimes = lines <| getSplitDistances newModel.distanceChosen
                            , lapTimesFloats = []
                            , infoMsg = errorMsg
                        }

                    ( _, floatList ) ->
                        { newModel
                            | lapTimes =
                                getLapTimesAsList
                                    { newModel | lapTimesFloats = floatList }
                                <|
                                    Maybe.withDefault "" (List.head newModel.splitTimes)
                            , lapTimesFloats = floatList
                            , infoMsg = errorMsg
                        }


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
            -- case model.splitTimes of
            --     [] ->
            --         (updateRounding model)
            --
            --     _ ->
            update CalculateButtonClicked (updateRounding model)

        DecimalLimiterClicked ->
            update CalculateButtonClicked (updateDecimalLimiter model)


updateDecimalLimiter model =
    { model
        | decimalLimiter =
            case model.decimalLimiter of
                Round ->
                    Truncate

                Truncate ->
                    Round
    }


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



-- lapInfoName : LapInfo -> String
-- lapInfoName lapInfo =
--     case lapInfo of
--         LapDistance ->
--             "Passeringsdistanse"
--
--         LapDifference ->
--             "Differanse"
--
--         LapTime ->
--             "Rundetid"
--
--         LapSpeed ->
--             "Hastighet"


createLapTimeTexts : Model -> List (Html msg)
createLapTimeTexts model =
    let
        strings =
            [ "Snittrundetid_____:  ", "Beste rundetid____: ", "Dårligste rundetid: " ]

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
                fixDecimalLength model.decimalLimiter model.rounding avgLapTime

        inputAreaValue =
            model.textContent
                |> lines
                |> List.take (getNrOfLaps model.distanceChosen)
                |> unlines

        nrOfCols =
            Basics.max 40 <| Maybe.withDefault 40 <| List.maximum <| List.map String.length <| lines lapText
    in
        div [] <|
            ([]
                -- ++ [ br [] [] ]
                -- ++ [ input [ maxlength 1, onInput FormatInput ] [] ]
                ++ [ br [] [] ]
                ++ model.distanceButtons
                ++ [ br [] [] ]
                ++ createCalculateButtons model
                ++ createLapTimeTexts model
                ++ [ br [] []
                   , unadjustableTextarea [ cols 3, rows nrOfLaps, placeholder <| unlines (List.map toString (List.range 1 nrOfLaps)) ] []
                   , unadjustableTextarea [ cols 15, rows nrOfLaps, onInput AreaInput, value inputAreaValue, placeholder "Tider her...." ] []
                   , unadjustableTextarea [ cols nrOfCols, rows nrOfLaps, readonly True, value lapText, styleTabSize ] []
                   , unadjustableTextarea [ cols 15, rows 25, readonly True, value testData10k ] []
                   ]
                ++ [ br [] []
                   , text "Velg formatering : "
                   , input [ style [ ( "width", "90px" ) ], onInput FormatInput, value <| String.fromList model.outputFormatString ] []
                   , createFormatInfo model
                   , br [] []
                   , br [] []
                   , unadjustableTextarea [ style [ ( "outline", "none" ), ( "border", "none" ) ], cols 81, rows <| 1 + List.length formatInfoText, readonly True, value <| String.join "\n\t" formatInfoText ] []
                   ]
            )


formatInfoText =
    [ "Velg formatering. Mulige valg:"
    , "d : distanse      - antall passerte meter for hver passering"
    , "T : rundeTid      - rundetid for runden"
    , "D : Differanse    - mellom snittrundetiden og hver rundetid"
    , "H : Hastighet     - hastighet for runden i km/t"
    , "P : Passeringstid - passeringstid for runden (samme som input)\nStandard er dTDH, som også vil bli brukt hvis det er ugyldig input"
    ]


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
                    "Ugyldig symbol : " ++ String.fromChar x
    in
        text infoTxt


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
        ]


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


getInfoMsgCss infoMsg =
    case infoMsg of
        Instruction _ ->
            instructionMsgStyle

        ErrorMsg _ ->
            errorMsgStyle

        WarningMsg _ ->
            warningMsgStyle


unadjustableTextarea : List (Attribute msg) -> List (Html msg) -> Html msg
unadjustableTextarea attributes htmls =
    textarea (Css.unadjustable :: attributes) htmls


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, update = update, view = view }
