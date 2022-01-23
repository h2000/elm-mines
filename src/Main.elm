module Main exposing (main)

import Helper exposing (..)
import List
import Playground exposing (..)
import String exposing (fromFloat)


main =
    game view update initialMemory


type alias Memory =
    { currentPos : Coord, selections : List Coord }


initialMemory : Memory
initialMemory =
    { currentPos = { x = 0.0, y = 0.0 }, selections = [] }


update : Computer -> Memory -> Memory
update computer mem =
    let
        pos =
            mousePos computer
    in
    { currentPos = pos
    , selections =
        if computer.mouse.click then
            insertAndRemoveDuplicates
                pos
                mem.selections

        else
            mem.selections
    }


view : Computer -> Memory -> List Shape
view computer mem =
    [ viewGame mem |> scale (gameScale computer)
    , viewHud computer mem
    ]


viewHud : Computer -> Memory -> Shape
viewHud computer mem =
    let
        pos =
            mem.currentPos |> (\{ x, y } -> fromFloat x ++ "," ++ fromFloat y)

        ps =
            mem.selections
                |> List.map (\{ x, y } -> fromFloat x ++ "," ++ fromFloat y)
                |> String.join " "
    in
    group
        [ words black (pos ++ "/" ++ ps)
        ]
        |> moveY (computer.screen.top - 10)


viewGame : Memory -> Shape
viewGame mem =
    group
        [ group (gridCoordinates |> List.map viewCell)
        , circle white 0.4
            |> move mem.currentPos.x mem.currentPos.y
            |> fade 0.3
        , group
            (mem.selections
                |> List.map
                    (\{ x, y } ->
                        rectangle white 0.95 0.95
                            |> move x y
                            |> fade 0.3
                    )
            )
        ]


viewCell : ( Int, Int ) -> Shape
viewCell ( x, y ) =
    group
        [ rectangle lightBlue 0.95 0.95
        , words white (String.fromInt x ++ "," ++ String.fromInt y)
            |> scale 0.019
        ]
        |> move (toFloat x) (toFloat y)
