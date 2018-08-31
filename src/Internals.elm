module Internals exposing (CPF(..), firstDV, secondDV)

import Extra.List exposing (zip)


type CPF
    = CPF (List Int) Int Int


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


firstDV : List Int -> Int
firstDV =
    dv


secondDV : Int -> List Int -> Int
secondDV dv1 numbers =
    List.singleton dv1
        |> List.append numbers
        |> dv
