module Main exposing (main)

import EverySet exposing (EverySet)
import Html exposing (a)
import List
import Playground exposing (..)


main =
    game view update initialMemory


type alias Memory =
    { currentPos : Coord
    , selections : List Coord
    }


type alias Coord =
    { x : Float, y : Float }


initialMemory : Memory
initialMemory =
    { currentPos = { x = 1.0, y = 1.0 }, selections = [] }


update : Computer -> Memory -> Memory
update computer mem =
    let
        { x, y } =
            toGameCoordinates computer computer.mouse

        convert z =
            z
                |> round
                |> toFloat
                |> clamp -constants.gridAbs constants.gridAbs

        pos =
            { x = convert x, y = y }
    in
    { currentPos = pos
    , selections =
        if computer.mouse.click then
            uniqueInsert pos mem.selections

        else
            mem.selections
    }


uniqueInsert : a -> List a -> List a
uniqueInsert x xs =
    EverySet.toList (EverySet.insert x (EverySet.fromList xs))


view : Computer -> Memory -> List Shape
view computer mem =
    [ group (viewGame mem) |> scale (gameScale computer)
    , viewHud computer mem
    , circle red 5
    ]


viewHud : Computer -> Memory -> Shape
viewHud computer mem =
    group
        [ words black (Debug.toString mem)
        ]
        |> moveY (computer.screen.top - 10)



-- |> move


viewGame : Memory -> List Shape
viewGame mem =
    [ group (coords |> List.map dot)
    , circle white 0.4
        |> move mem.currentPos.x mem.currentPos.y
        |> fade 0.3
    , group
        (mem.selections
            |> List.map
                (\{ x, y } ->
                    circle yellow 0.4
                        |> move x y
                        |> fade 0.3
                )
        )
    ]


dot : ( Int, Int ) -> Shape
dot ( x, y ) =
    group
        [ rectangle lightBlue 0.95 0.95 |> fade 0.75
        , words white (String.fromInt x ++ "," ++ String.fromInt y)
            |> scale 0.019
        ]
        |> move (toFloat x) (toFloat y)


coords : List ( Int, Int )
coords =
    let
        row c =
            List.range -5 5 |> List.map (\r -> ( r, c ))
    in
    List.range -5 5 |> List.concatMap row


constants : { gameWidth : number, gridAbs : number }
constants =
    { gameWidth = 12
    , gridAbs = 5
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
