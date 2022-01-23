module CreekType where

-- BoardSize: (width, height), describe board size
type BoardSize = (Int, Int)

-- Coordinates: (x-axis, y-axis), describes coordinates on board
type Coordinate = (Int, Int)

-- CoordValue: (x-axis, y-axis) value, describes how many fileds have to be coloured by given value
-- np. (1,1), 3
type CoordValue = (Coordinate, Int)

-- Creek: board, size of board and list of fields to colour
data Creek = Creek BoardSize [CoordValue]
    deriving (Show, Read)

-- CoordColoured: (x-axis, y-axis) boolValue, describes if fields with given coords have to be coloured
type CoordColoured = (Coordinate, Bool)

-- SolvedBoard
type SolvedBoard = [CoordColoured]
