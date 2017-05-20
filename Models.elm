module Models exposing (..)

import Html exposing (Html)


type Distance
    = D500
    | D1000
    | D1500
    | D3000
    | D5000
    | D10000


type alias Model =
    { textContent : String
    , distanceChosen : Distance
    , splitTimes : List String
    , lapTimes : List String
    , lapTimesFloats : List Float
    , infoMsg : InfoMsg
    , rounding : RoundingType
    , distanceButtons : List (Html Msg)
    , outputFormatString : List Char
    , delimiter : Char
    , decimalLimiter : DecimalLimiter
    }


type Msg
    = AreaInput String
    | FormatInput String
    | DistanceButtonClicked Distance
    | CalculateButtonClicked
    | RoundingButtonClicked
    | DecimalLimiterClicked
    | TestDataButtonClicked String


type DecimalLimiter
    = Round
    | Truncate


type InfoMsg
    = Instruction String
    | ErrorMsg String
    | WarningMsg String


type alias DataModel =
    {}


type Status
    = Blank
    | Ready (List Float)
    | Calculated (List Float) InfoMsg


type LapInfo
    = LapDistance
    | LapDifference
    | LapTime
    | LapSpeed
    | LapSplitTime


type RoundingType
    = ZeroDecimal
    | OneDecimal
    | TwoDecimal
