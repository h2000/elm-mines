module MainSpec exposing (suite)

import Expect
import Test exposing (..)


suite : Test
suite =
    Test.describe "Test Spec"
        [ Test.test "Test1" <|
            \_ ->
                1 |> Expect.equal 1
        ]
