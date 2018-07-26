module Internals exposing (CPF(..), create, validate)

import Extra.List exposing (zip)


type CPF
    = CPF (List Int) Int Int


mod =
    flip rem


dv numbers =
    let
        f ( index, digit ) =
            digit * (9 - rem index 10)
    in
        List.reverse numbers
            |> zip (List.range 0 <| List.length numbers)
            |> List.map f
            |> List.sum
            |> mod 11
            |> mod 10


fstDV =
    dv


sndDV dv1 numbers =
    List.singleton dv1
        |> List.append numbers
        |> dv


validate : List Int -> Int -> Int -> Result String CPF
validate numbers dv1 dv2 =
    if List.length numbers /= 9 then
        Err "Invalid CPF length."
    else if dv1 /= fstDV numbers then
        Err "The first DV is wrong!"
    else if dv2 /= sndDV dv1 numbers then
        Err "The second DV is wrong!"
    else
        Ok (CPF numbers dv1 dv2)


create : List Int -> CPF
create numbers =
    let
        dv1 =
            fstDV numbers
    in
        CPF numbers dv1 (sndDV dv1 numbers)
