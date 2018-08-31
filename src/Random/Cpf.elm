module Random.Cpf exposing (cpf)

{-| Generate a random CPF

@docs cpf

-}

import Cpf exposing (CPF)
import Internals exposing (firstDV, secondDV)
import Random exposing (Generator)


create numbers =
    let
        dv1 =
            firstDV numbers
    in
    Internals.CPF numbers dv1 (secondDV dv1 numbers)


{-| Generate a random CPF
-}
cpf : Generator CPF
cpf =
    Random.int 0 9
        |> Random.list 9
        |> Random.map create
