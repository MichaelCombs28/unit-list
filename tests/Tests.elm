module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import UnitList exposing (..)


all : Test
all =
    describe "UnitList test suite"
        [ describe "UnitList tests."
            [ test "isSingleton" <|
                \() ->
                    singleton 1
                        |> push 2
                        |> isSingleton
                        |> Expect.false "Should be false."
            , test "Initialize unitlist" <|
                \() ->
                    singleton 0
                        |> updateHistory (always [ 1, 2, 3, 4 ])
                        |> initialize
                        |> history
                        |> Expect.equal []
            , test "Archive unitlist" <|
                \() ->
                    fromList 1 [ 2, 3, 4, 5 ]
                        |> archive 6
                        |> history
                        |> Expect.equal [ 1, 2, 3, 4, 5 ]
            , test "Next" <|
                \() ->
                    let
                        ulist =
                            fromList 1 [ 2, 3, 4, 5 ]
                                |> next
                    in
                        fromList 2 [3,4,5]
                            |> updateHistory (always [1])
                            |> Expect.equal ulist

            , test "Previous" <|
                \() ->
                    let
                        ulist =
                            fromList 1 [2,3,4,5]
                                |> next
                                |> previous
                    in
                        fromList 1 [2,3,4,5]
                            |> Expect.equal ulist

            , test "Step positive" <|
                \() ->
                    let
                        ulist =
                            fromList 1 [2,3,4,5]
                                |> step 1
                    in
                        fromList 1 [2,3,4,5]
                            |> next
                            |> Expect.equal ulist

            , test "Step negative" <|
                \() ->
                    let
                        ulist =
                            fromList 1 [2,3,4,5]
                                |> updateHistory (always [3,1,2])
                                |> step -1
                    in
                        fromList 1 [2,3,4,5]
                            |> updateHistory (always [3,1,2])
                            |> previous
                            |> Expect.equal ulist

            ]
        ]
