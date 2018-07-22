module Cpf exposing (CPF)

{-| Manipulate and generate brazilian CPFs
@docs CPF
-}

import List exposing (..)


{-| A brazilian CPF
-}
type CPF
    = CPF (List Int) Int Int
