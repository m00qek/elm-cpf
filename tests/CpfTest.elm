module CpfTest exposing (text, validation)

import Cpf
import Expect
import Internals
import Test exposing (Test, describe, test)


cpf =
    Internals.CPF [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] 0 9


validation : Test
validation =
    describe "CPF creation and validation"
        [ test "Should create a CPF from a valid list of numbers" <|
            \_ ->
                Cpf.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 9 ] |> Expect.equal (Ok cpf)
        , test "Should not create a CPF with missing digits" <|
            \_ ->
                Cpf.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] |> Expect.equal (Err Cpf.InvalidLength)
        , test "Should not create a CPF with invalid first 'Digito Verificador'" <|
            \_ ->
                Cpf.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 9 ] |> Expect.equal (Err Cpf.InvalidFirstDV)
        , test "Should not create a CPF with invalid second 'Digito Verificador'" <|
            \_ ->
                Cpf.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0 ] |> Expect.equal (Err Cpf.InvalidSecondDV)
        ]


text : Test
text =
    describe "CPFs from/to text"
        [ test "Should create a CPF from a valid numeric text" <|
            \_ ->
                Cpf.fromText "12345678909" |> Expect.equal (Ok cpf)
        , test "Should create a CPF from a valid text" <|
            \_ ->
                Cpf.fromText "123.456.789-09" |> Expect.equal (Ok cpf)
        , test "Should convert a CPF to string" <|
            \_ ->
                Cpf.toString cpf |> Expect.equal "12345678909"
        , test "Should pretty print a CPF" <|
            \_ ->
                Cpf.show cpf |> Expect.equal "123.456.789-09"
        ]
