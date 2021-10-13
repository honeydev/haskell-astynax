{-# LANGUAGE StandaloneKindSignatures #-}

module Columns where
import Data.List

-- data CoordinateType = X | Y
-- data Coordinate = Coordinate CoordinateType Int Int
-- data Player = Red | Blue
-- data Row = Row Coordinate Coordinate Coordinate Coordinate
-- 
-- showCoordinate :: Coordinate -> String
-- showCoordinate c 
--   = let format coordinateType value = coordinateType ++ (show value)
--   in case c of
--     Coordinate X n -> format "X" n
--     Coordinate Y n -> format "Y" n
-- 
data Err = InvalidCoordinates   
data Player = Red | Blue

playerStr :: Player -> String
playerStr p = case p of
  Red  -> "R"
  Blue -> "B"

data Draught = Draught Int

draughtCount :: Draught -> Int
draughtCount (Draught draught) = draught 

data CellState = Occupied Player Draught | Empty

cellPlayer :: CellState -> Player
cellPlayer (Occupied p _) = p

cellDraught :: CellState -> Int
cellDraught (Occupied _ d) = draughtCount d

-- occupiedPlayer :: CellState -> Player
-- occupiedPlayer (Occupied p _) = p
-- 
-- occupiedDraught :: Occupied -> Draught
-- occupiedDraught (Occupied _ d) = d

data Cell = Cell
  { occupied :: CellState
  , x :: Int
  , y :: Int
  }

cellX :: Cell -> Int
cellX (Cell _ x _) = x

cellY :: Cell -> Int
cellY (Cell _ _ y) = y

cellState :: Cell -> CellState
cellState (Cell state _ _) = state


cellView :: Cell -> String
cellView (Cell state _ _) = case state of
  (Occupied p d) -> "[" ++ playerStr p ++ show (draughtCount d) ++ "]"
  Empty -> "[  ]"

data Row = Row [Cell]
data Deck = Deck [Row]

initRow :: (Int -> Cell) -> Row 
initRow coordinateFactory = Row (map coordinateFactory [1..4])  

initDeck :: Deck
initDeck =
  let blue = initRow (\n -> Cell { occupied = Occupied Blue (Draught 1), x = n, y = 1 })
      red = initRow (\n -> Cell { occupied = Occupied Red (Draught 1), x = n, y = 4 })
      initEmptyRow y = initRow (\n -> Cell { occupied = Empty, x = n, y = y })
  in Deck [
    red, 
    initEmptyRow 2, 
    initEmptyRow 3, 
    blue
  ]

move deck sourceCell targetCell =
  let (Deck rows) = deck
      (Cell sourceCellState sourceX sourceY) = sourceCell
      (Cell targetCellState targetX targetY) = targetCell
      newDraught = ((cellDraught sourceCellState) + (cellDraught targetCellState))
      targetCell =  Cell { occupied = Occupied (cellPlayer sourceCellState) (Draught newDraught), x = targetX, y = targetY }
  in newDraught

renderRow (Row cells) = print (concat (intersperse " " (map cellView cells)))
renderDeck (Deck rows) = mapM_ renderRow rows



s = Cell { occupied = Occupied Blue (Draught 1), x = 1, y = 1 }
t = Cell { occupied = Occupied Blue (Draught 1), x = 2, y = 1 }

-- move initDeck s t
