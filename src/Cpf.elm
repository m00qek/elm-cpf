module Cpf exposing (CPF, Error, fromList, fromText, toString, show)

{-| Manipulate and generate brazilian CPFs

@docs CPF, Error, fromList, fromText, toString, show

-}

import Char
import Control.Applicative exposing (liftA3)
import Extra.List exposing (cons, dropLast, last, partition, penultimate)
import Internals exposing (validate)
import List
import String


{-| A brazilian CPF
-}
type alias CPF =
    Internals.CPF


{-| Possible errors when dealing with CPFs
-}
type alias Error =
    Internals.Error


{-| Turn a valid list of integers into a CPF.

    (fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 9 ] |> Result.map show) == Ok "123.456.789-09"

-}
fromList : List Int -> Result Error CPF
fromList =
    liftA3 (Maybe.map3 validate) (Just << dropLast 2) penultimate last
        >> Result.fromMaybe Internals.InvalidLength
        >> Result.andThen identity


{-| Turn a valid string into a CPF.

    (fromText "12345678909" |> Result.map show) == Ok "123.456.789-09"

    (fromText "123.456.789-09" |> Result.map show) == Ok "123.456.789-09"

-}
fromText : String -> Result Error CPF
fromText =
    String.toList
        >> List.filter Char.isDigit
        >> List.map String.fromChar
        >> List.map String.toInt
        >> List.map (Result.fromMaybe Internals.InvalidInput)
        >> List.foldr (Result.map2 cons) (Ok [])
        >> Result.andThen fromList


{-| Turn a CPF into a string.
-}
toString : CPF -> String
toString (Internals.CPF numbers dv1 dv2) =
    (numbers ++ [ dv1, dv2 ])
        |> List.map String.fromInt
        |> String.concat


{-| Pretty print a given CPF
-}
show : CPF -> String
show (Internals.CPF numbers dv1 dv2) =
    let
        withDots =
            List.concat << List.intersperse [ "." ] << Extra.List.partition 3
    in
    String.concat
        [ List.map String.fromInt numbers |> withDots |> String.concat
        , "-"
        , List.map String.fromInt [ dv1, dv2 ] |> String.concat
        ]
