module Cpf exposing (CPF, fromList, toString)

{-| Manipulate and generate brazilian CPFs
@docs CPF, fromList, toString
-}

import List exposing (append, length, map, range, reverse, singleton, sum)
import Extra.List exposing (..)
import Applicative.Function exposing (liftA3)


{-| A brazilian CPF
-}
type CPF
    = CPF (List Int) Int Int


mod =
    flip rem


dv numbers =
    let
        f ( index, digit ) =
            digit * (9 - rem index 10)
    in
        reverse numbers
            |> zip (range 0 <| length numbers)
            |> map f
            |> sum
            |> mod 11
            |> mod 10


fstDV =
    dv


sndDV dv1 numbers =
    singleton dv1
        |> append numbers
        |> dv


create numbers dv1 dv2 =
    if length numbers /= 9 then
        Err "Invalid CPF length."
    else if dv1 /= fstDV numbers then
        Err "The first DV is wrong!"
    else if dv2 /= sndDV dv1 numbers then
        Err "The second DV is wrong!"
    else
        Ok (CPF numbers dv1 dv2)


{-| Turn a valid list of integers into a CPF.
(fromList [1,2,3,4,5,6,7,8,9,0,9] |> Result.map toString) == Ok "12345678909"
-}
fromList : List Int -> Result String CPF
fromList =
    liftA3 (Maybe.map3 create) (Just << dropLast 2) penultimate last
        >> Result.fromMaybe "Invalid input."
        >> Result.andThen identity


{-| Turn a CPF into a string.
-}
toString : CPF -> String
toString (CPF numbers dv1 dv2) =
    (numbers ++ [ dv1, dv2 ])
        |> map Basics.toString
        |> String.concat
