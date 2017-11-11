module Models exposing (..)

import Html exposing (Html)


--import Array exposing (Array)


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
    , currentMode : Mode
    , lapProgressionString : String
    , formatButtons : List FormatButton
    }


type Msg
    = AreaInput String
    | FormatInput String
    | DistanceButtonClicked Distance
    | CalculateButtonClicked
    | RoundingButtonClicked
    | DecimalLimiterClicked
    | TestDataButtonClicked String
    | ModeButtonClicked
    | LapProgressionInput String
    | FormatButtonClicked FormatButton
    | FormatButtonOrderClicked FormatButton Dir


type Dir
    = Up
    | Down


type alias Mode =
    { modeType : ModeType
    , infoWhenBlank : String
    , checkInput : List String -> Result InfoMsg (List String)
    , getLapTimes : ModelArg -> Result InfoMsg (List Float)
    }


type ModelArg
    = ModelArg Model


type alias LapProgessionData =
    { lapProgression : Float }


type ModeType
    = SplitTimesMode
    | LapTimeMode Float


type DecimalLimiter
    = Round
    | Truncate


type InfoMsg
    = Instruction String
    | ErrorMsg String
    | WarningMsg String


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


type alias FormatButton =
    { lapInfo : LapInfo
    , on : Bool
    , index : Float
    }
