module Main exposing (main)

import Lib.Helper exposing (..)
import Lib.Playground exposing (..)
import List
import String exposing (fromFloat)


main : GameProgram Memory
main =
    game view update initialMemory


type alias Memory =
    { currentPos : Coord
    , selections : List Coord
    , randomNrs : List Int
    }


initialMemory : Memory
initialMemory =
    { currentPos = { x = 0.0, y = 0.0 }
    , selections = []
    , randomNrs = []
    }


update : Computer -> Memory -> Memory
update computer mem =
    let
        pos =
            mousePos computer
    in
    { currentPos = pos
    , selections =
        if computer.mouse.click then
            Debug.log "XXX" <|
                insertAndRemoveDuplicates
                    pos
                    mem.selections

        else
            mem.selections
    , randomNrs =
        if List.isEmpty mem.randomNrs && (toMillis computer.time > 0) then
            randomNrs 10 1 10 (initSeed computer)

        else
            mem.randomNrs
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

        nrs =
            "Random: "
                ++ String.join " " (mem.randomNrs |> List.map String.fromInt)
    in
    group
        [ words black (pos ++ "/" ++ ps) |> moveY (computer.screen.top - 10)
        , words black nrs |> moveY (computer.screen.top - 30)
        ]


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
