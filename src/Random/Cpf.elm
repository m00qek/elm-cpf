module Random.Cpf exposing (cpf)

import Random exposing (Generator)
import Cpf exposing (CPF)
import Internals


{-| Generate a random CPF
-}
cpf : Generator CPF
cpf =
    Random.int 0 9
        |> Random.list 9
        |> Random.map Internals.create
