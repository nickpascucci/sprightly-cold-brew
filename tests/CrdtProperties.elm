module CrdtProperties exposing (..)

import Crdt exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set exposing (insert, singleton)
import Test exposing (..)


suite : Test
suite =
    describe "The CRDT type"
        [ describe "AppendOnlySet of a Sequenced value"
            [ test "can be reduced to a single, latest value" <|
                \_ ->
                    let
                        aos : AppendOnlySet (Sequenced String)
                        aos =
                            AOS (insert ( 1, "Bar" ) (singleton ( 0, "Foo" )))
                    in
                    Expect.equal (Just "Bar") (latest aos)
            ]
        , describe "Counter"
            [ test "can be incremented from zero" <|
                \_ ->
                    zero
                        |> increment 123
                        |> count
                        |> Expect.equal 1
            , test "will ignore non-unique increment markers" <|
                \_ ->
                    zero
                        |> increment 123
                        |> increment 123
                        |> count
                        |> Expect.equal 1
            , test "will not ignore unique increment markers" <|
                \_ ->
                    zero
                        |> increment 123
                        |> increment 321
                        |> count
                        |> Expect.equal 2
            , test "can be decremented to 0" <|
                \_ ->
                    zero
                        |> increment 123
                        |> decrement 321
                        |> count
                        |> Expect.equal 0
            , test "can be decremented past 0" <|
                \_ ->
                    zero
                        |> decrement 123
                        |> decrement 321
                        |> count
                        |> Expect.equal -2
            ]
        ]
