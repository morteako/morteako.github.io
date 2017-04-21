module Models exposing (..)

import Utils exposing (RoundingType)


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
    }


type Msg
    = Input String
    | DistanceButtonClicked Distance
    | CalculateButtonClicked
    | RoundingButtonClicked


type InfoMsg
    = Instruction String
    | ErrorMsg String
