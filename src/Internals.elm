module Internals exposing (CPF(..), Error(..), create, validate)

import Extra.List exposing (zip)


type CPF
    = CPF (List Int) Int Int


type Error
    = InvalidInput
    | InvalidLength
    | InvalidFirstDV
    | InvalidSecondDV


dv numbers =
    let
        f ( index, digit ) =
            digit * (9 - remainderBy 10 index)
    in
    List.reverse numbers
        |> zip (List.range 0 <| List.length numbers)
        |> List.map f
        |> List.sum
        |> remainderBy 11
        |> remainderBy 10


fstDV =
    dv


sndDV dv1 numbers =
    List.singleton dv1
        |> List.append numbers
        |> dv


validate : List Int -> Int -> Int -> Result Error CPF
validate numbers dv1 dv2 =
    if List.length numbers /= 9 then
        Err InvalidLength

    else if dv1 /= fstDV numbers then
        Err InvalidFirstDV

    else if dv2 /= sndDV dv1 numbers then
        Err InvalidSecondDV

    else
        Ok (CPF numbers dv1 dv2)


create : List Int -> CPF
create numbers =
    let
        dv1 =
            fstDV numbers
    in
    CPF numbers dv1 (sndDV dv1 numbers)
