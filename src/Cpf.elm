module Cpf exposing (CPF, fromList, toString)

{-| Manipulate and generate brazilian CPFs
@docs CPF, fromList, toString
-}

import Applicative.Function exposing (liftA3)
import Extra.List exposing (dropLast, last, partition, penultimate, zip)
import List


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


create numbers dv1 dv2 =
    if List.length numbers /= 9 then
        Err "Invalid CPF length."
    else if dv1 /= fstDV numbers then
        Err "The first DV is wrong!"
    else if dv2 /= sndDV dv1 numbers then
        Err "The second DV is wrong!"
    else
        Ok (CPF numbers dv1 dv2)


{-| Turn a valid list of integers into a CPF.
(fromList [1,2,3,4,5,6,7,8,9,0,9] |> Result.map show) == Ok "123.456.789-09"
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
        |> List.map Basics.toString
        |> String.concat


{-| Pretty print a given CPF
-}
show : CPF -> String
show (CPF numbers dv1 dv2) =
    let
        withDots =
            List.concat << List.intersperse [ "." ] << Extra.List.partition 3
    in
        String.concat
            [ List.map Basics.toString numbers |> withDots |> String.concat
            , "-"
            , List.map Basics.toString [ dv1, dv2 ] |> String.concat
            ]
