
module Main where

import Data.List

data Cell = White | Black | Empty
type Row = [Cell]
type Grid = [Row]

instance Eq Cell where
  Black == Black = True
  White == White = True
  Empty == Empty = True
  _ == _ = False

instance Show Cell where
  show Black = "○"
  show White = "●"
  show Empty = "┼"

{-
 - Create a Go grid given a size
 -
 - replicate 3 4 = [4,4,4]
 - (replicate 3 . replicate 4) 5 = [[5,5,5,5],[5,5,5,5],[5,5,5,5]]
 -
 - The grid is filled with Empty cells
 - -}

makeGrid :: Int -> Grid
makeGrid size = (replicate size . replicate size) Empty

{-
 - Convert a grid to string
 -
 - unlines ["hello", "world"] -- "hello\nworld"
 - RowToString prepend the index on each row and returns a string
 - columns is a list of indexes
 - -}
gridToString :: Grid -> String
gridToString grid = unlines (["  " ++ columns] ++ (mapInd rowToString grid))
  where rowToString row index = (id . show $ index) ++ " " ++ (intercalate " " $ map show row)
        columns = intercalate " " $ zipWith (\_ i -> id . show $ i) (grid !! 0) [0..]

{-
 - Return the value at position (x, y) in the Grid
 - -}
getValue :: Grid -> Int -> Int -> Cell
getValue grid x y = (grid !! y) !! x

{-
 - Returns True if the position is a valid coordinate
 - in the Grid, False otherwise
 - -}
isInBoundaries :: Grid -> Int -> Int -> Bool
isInBoundaries grid x y = x >= 0 && x < length (grid !! 0) && y >= 0 && y < length grid

{-
 - Replaces a cell at a given position,
 - then returns the Grid
 - -}
replaceCell :: Cell -> Int -> Int -> Grid -> Grid
replaceCell value x y (r:rs)
  | y == 0 = [replaceRowNth r x value] ++ rs
  | otherwise = [r] ++ replaceCell value x (y - 1) rs
  where replaceRowNth
          = (\(r:rs) n value -> if n == 0 then [value] ++ rs else [r] ++ replaceRowNth rs (n - 1) value)

{-
 - Check if a cell has a liberty within a group of cell.
 -
 - If the cell is Empty, returns True
 -
 - If the cell above or below or on the right, or on the left
 -  has a liberty, return True
 -
 - If the cell has already been checked, return False-}
hasLiberty :: Cell -> [(Int, Int)] -> Grid -> Int -> Int -> Bool
hasLiberty cell state grid x y
  | isInBoundaries grid x y == False || (value /= Empty && value /= cell) || alreadyChecked = False
  | otherwise = value == Empty || above || below || right || left
  where value = getValue grid x y
        alreadyChecked = any (\pos -> fst pos == x && snd pos == y) state
        check = hasLiberty cell (state ++ [(x, y)]) grid
        above = check x (y - 1)
        below = check x (y + 1)
        right = check (x + 1) y
        left = check (x - 1) y

{-
 - Can the player play in a given position?
 -
 - we compute the new Grid if the player plays,
 -  then if we find out that the new grid remove the liberties of the player,
 -  return False
 - -}
canPlay :: Cell -> Int -> Int -> Grid -> Bool
canPlay cell x y grid
  | isInBoundaries grid x y == False = False
  | otherwise = value == Empty && hasLiberty cell [] newGrid x y
  where value = getValue grid x y
        newGrid = cleanGrid (other cell) [] (replaceCell cell x y grid) 0 0

{-
 - Return the oponent color
 - -}
other :: Cell -> Cell
other Empty = error "Should not call other on Empty"
other White = Black
other Black = White

{-
 - Map by giving the index as well as the value
 -  to the function
 - -}
mapInd f l = zipWith f l [0..]

{-
 - Takes a Cell (the color to clean), a state of visited positions,
 -  a grid and a position, and return a new grid after removing all the
 -  cells that do not have any liberty
 -
 - To do so, if a cell has a liberty given our state,
 - return the grid and do not remove anything, other wise, add the cell position
 - to the state, and cleanGrid above, below, on the right and left
 -
 - before cleaning the current cell and go on
 - -}
cleanGrid cell state grid x y
  | isInBoundaries grid x y == False = grid
  | value /= cell || alreadyChecked || hasLiberty cell state grid x y
       = cleanGrid cell state (cleanGrid cell state grid (x + 1) y) x (y + 1)
  | otherwise = current . left . right . above . below $ grid
  where alreadyChecked = any (\pos -> fst pos == x && snd pos == y) state
        value = getValue grid x y
        clean = cleanGrid cell (state ++ [(x, y)])
        current g = replaceCell Empty x y grid
        above g = clean g x (y - 1)
        below g = clean g x (y + 1)
        right g = clean g (x - 1) y
        left g = clean g (x + 1) y

{-
 - Loop play is the Color that needs to play
 - -}
loop grid play = do
  putStrLn ""
  putStrLn . gridToString $ grid
  putStrLn ""
  putStrLn (show play ++ " plays")
  putStrLn ""
  putStr "X: "
  x <- getLine
  putStr "Y: "
  y <- getLine
  let x' = read x :: Int
  let y' = read y :: Int
  if canPlay play x' y' grid
    then loop (cleanGrid (other play) [] (replaceCell play x' y' grid) 0 0) (other play)
    else do
      print "You cannot play"
      loop grid play

main = loop (makeGrid 6) White
