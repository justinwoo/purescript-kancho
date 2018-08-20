module Generated exposing (..)

-- coords JS: { x: 1, y: 2 }
-- etchSketch JS: { cursor: { x: 1, y: 2 }, points: [] }
-- thing JS: { a: true, b: 0.30000000000000004, c: "c" }


type alias Coords =
    { x : Int
    , y : Int
    }


type alias EtchSketch =
    { cursor : Coords
    , points : List Coords
    }


type alias Thing =
    { x : Int
    , y : Int
    }
