module Lib.Helper exposing (..)

import EverySet
import Lib.Playground exposing (Computer, Mouse, toMillis)
import Random


type alias Coord =
    { x : Float, y : Float }


gridCoordinates : List ( Int, Int )
gridCoordinates =
    let
        row col =
            List.range -constants.gridAbs constants.gridAbs
                |> List.map (\r -> ( r, col ))
    in
    List.range -constants.gridAbs constants.gridAbs
        |> List.concatMap row


constants : { gameWidth : number, gridAbs : number }
constants =
    let
        side =
            5
    in
    { gameWidth = side * 2 + 2
    , gridAbs = side
    }


gameScale : { a | screen : { b | width : Float } } -> Float
gameScale computer =
    computer.screen.width / (constants.gameWidth + 1.6)


toGameCoordinates : Computer -> Mouse -> { x : Float, y : Float }
toGameCoordinates computer { x, y } =
    let
        k =
            gameScale computer
    in
    { x = 1 / k * x
    , y = 1 / k * y
    }


mousePos : Computer -> Coord
mousePos computer =
    let
        { x, y } =
            toGameCoordinates computer computer.mouse

        convert z =
            z
                |> round
                |> toFloat
                |> clamp -constants.gridAbs constants.gridAbs
    in
    { x = convert x, y = convert y }


insertAndRemoveDuplicates : a -> List a -> List a
insertAndRemoveDuplicates x xs =
    EverySet.toList (EverySet.insert x (EverySet.fromList xs))


initSeed : Computer -> Random.Seed
initSeed computer =
    let
        seed =
            toMillis computer.time
    in
    Debug.log ("Time:" ++ Debug.toString seed) Random.initialSeed seed


randomNrs : Int -> Int -> Int -> Random.Seed -> List Int
randomNrs count min max seed =
    Random.step (Random.list count (Random.int min max)) seed |> Tuple.first
