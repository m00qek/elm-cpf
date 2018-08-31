module Random.Cpf exposing (cpf)

{-| Generate a random CPF

@docs cpf

-}

import Cpf exposing (CPF)
import Internals
import Random exposing (Generator)


{-| Generate a random CPF
-}
cpf : Generator CPF
cpf =
    Random.int 0 9
        |> Random.list 9
        |> Random.map Internals.create
