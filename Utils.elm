module Utils exposing (..)

import Regex


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



-- resultFoldrFuncs result funcs =
--     List.foldr
--         (\f g ->
--             case (f result) of
--                 Ok v ->
--                     g result
--         )
--         (Ok 1)
--         funcs


lines : String -> List String
lines ss =
    String.split "\n" ss


unlines : List String -> String
unlines ss =
    String.join "\n" ss


type RoundingType
    = OneDecimal
    | TwoDecimal


getRoundingFactor rounding =
    case rounding of
        OneDecimal ->
            10.0

        TwoDecimal ->
            100.0


getNumberOfZeroes rounding =
    case rounding of
        OneDecimal ->
            1

        TwoDecimal ->
            2


roundToDec : RoundingType -> Float -> String
roundToDec roundingType number =
    number
        |> (*) (getRoundingFactor roundingType)
        |> round
        |> toFloat
        |> (\x -> x / (getRoundingFactor roundingType))
        |> toString


map2Full f2 f1 xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            List.map f1 ys

        ( _, [] ) ->
            List.map f1 xs

        ( x :: xs, y :: ys ) ->
            f2 x y :: map2Full f2 f1 xs ys


findIndex : (a -> Bool) -> List a -> Maybe number
findIndex pred xs =
    let
        index =
            List.foldr
                (\x y ->
                    if pred x then
                        Just 0
                    else
                        addMaybe (Just 1) y
                )
                Nothing
                xs
    in
        index


average : List Float -> Float
average nrs =
    List.sum nrs / toFloat (List.length nrs)


tupleMap3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tupleMap3 f ( a, b, c ) =
    ( f a, f b, f c )


tuple3ToList : ( a, a, a ) -> List a
tuple3ToList ( a, b, c ) =
    [ a, b, c ]


maybeFuncWithDefault f default =
    \x -> Maybe.withDefault default (f x)
