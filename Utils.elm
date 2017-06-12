module Utils exposing (..)

import Regex
import Models exposing (..)


replaceChar : Char -> Char -> String -> String
replaceChar a b xs =
    String.map
        (\x ->
            if a == x then
                b
            else
                x
        )
        xs


countFreq : a -> List a -> number
countFreq a xs =
    List.foldr
        (\x y ->
            if x == a then
                1 + y
            else
                y
        )
        0
        xs


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys


combine : List a -> List ( a, a )
combine xs =
    zip xs (Maybe.withDefault [] <| List.tail xs)


replaceRegexWith : String -> String -> String -> String
replaceRegexWith pat replPat str =
    Regex.replace Regex.All (Regex.regex pat) (\_ -> replPat) str


uncurryFlip : (a -> b -> c) -> (( b, a ) -> c)
uncurryFlip =
    uncurry << flip


addMaybe : Maybe number -> Maybe number -> Maybe number
addMaybe x y =
    case ( x, y ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just a, Just b ) ->
            Just <| a + b


untilScan : (a -> Bool) -> (a -> a) -> (a -> b) -> a -> List b
untilScan pred next f v =
    case pred v of
        False ->
            f v :: untilScan pred next f (next v)

        True ->
            []


accumulate op pred term next v default =
    if pred (term v) then
        default
    else
        op (term v) <| accumulate op pred term next (next v) default


dotProduct : List Float -> List Float -> Float
dotProduct xs ys =
    List.foldr (\x y -> (uncurry (*) <| x) + y) 0.0 (zip xs ys)


resultMap : (a -> Result error b) -> List a -> Result error (List b)
resultMap f xs =
    let
        iter : List b -> List a -> Result error (List b)
        iter acc curXs =
            case curXs of
                [] ->
                    Result.Ok acc

                x :: rest ->
                    case f x of
                        Err e ->
                            Err e

                        Ok v ->
                            iter (v :: acc) rest

        res =
            iter [] xs
    in
        case res of
            Err e ->
                Err e

            Ok vs ->
                Ok <| List.reverse vs


resultFoldr f v results =
    List.foldr
        (\x y ->
            case ( x, y ) of
                ( Ok v, Ok u ) ->
                    f u v

                ( Err e, _ ) ->
                    Err e

                ( _, Err e ) ->
                    Err e
        )
        v
        results


last : List a -> Maybe a
last =
    List.head << List.reverse


lines : String -> List String
lines ss =
    String.split "\n" ss


unlines : List String -> String
unlines ss =
    String.join "\n" ss


getRoundingFactor rounding =
    case rounding of
        ZeroDecimal ->
            1.0

        OneDecimal ->
            10.0

        TwoDecimal ->
            100.0


getNumberOfZeroes rounding =
    case rounding of
        ZeroDecimal ->
            0

        OneDecimal ->
            1

        TwoDecimal ->
            2


roundToDec : DecimalLimiter -> RoundingType -> Float -> String
roundToDec lim roundingType number =
    number
        |> (*) (getRoundingFactor roundingType)
        |> (case lim of
                Round ->
                    round

                _ ->
                    floor
           )
        |> toFloat
        |> (\x -> x / (getRoundingFactor roundingType))
        |> toString



--map2 without truncation


map2Full f2 fx fy xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            List.map fy ys

        ( _, [] ) ->
            List.map fx xs

        ( x :: xs, y :: ys ) ->
            f2 x y :: map2Full f2 fx fy xs ys


map2FullId f2 xs ys =
    map2Full f2 identity identity xs ys


findIndex : (a -> Bool) -> List a -> Maybe number
findIndex pred xs =
    List.foldr
        (\x y ->
            if pred x then
                Just 0
            else
                addMaybe (Just 1) y
        )
        Nothing
        xs


average : List Float -> Float
average nrs =
    List.sum nrs / toFloat (List.length nrs)


safeAverage : List Float -> Maybe Float
safeAverage nrs =
    case nrs of
        [] ->
            Nothing

        _ ->
            Just <| (List.sum nrs) / toFloat (List.length nrs)


tupleMap2 : (a -> b) -> ( a, a ) -> ( b, b )
tupleMap2 f ( a, b ) =
    ( f a, f b )


tupleMap3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tupleMap3 f ( a, b, c ) =
    ( f a, f b, f c )


tuple3ToList : ( a, a, a ) -> List a
tuple3ToList ( a, b, c ) =
    [ a, b, c ]


maybeFuncWithDefault f default =
    \x -> Maybe.withDefault default (f x)


listPadRight nr val xs =
    xs ++ List.repeat (nr - List.length xs) val
