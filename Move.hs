module Move ( updateBoard
             , move
             , checkIsMoveValid
             , deleteFromBoard
             , Move(..)
             , standardToQueen
             ) where

import Board

data Move = Step Position Position | Hit Position Position deriving Eq

updateBoard :: [[a]] -> a -> (Int, Int) -> [[a]]
updateBoard m x (c,r) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

move :: Board -> Move -> Board
move board (Hit (x1,y1) (x2,y2)) = let board' = let board'' = (updateBoard board (get (x1,y1) board) (x2,y2))
                                                in (deleteFromBoard board'' (x1,y1))
                                   in (deleteFromBoard board' (((x1+x2) `div` 2),((y1+y2) `div` 2)))
move board (Step (x1,y1) (x2,y2))
    | ((checkIsMoveValid (x1,y1)(x2,y2) board) == True) =
        let board' = (updateBoard board (get (x1,y1) board) (x2,y2))
        in (deleteFromBoard board' (x1,y1))
    | otherwise = board

checkIsMoveValid :: (Ord a, Num a) => Position -> (Int, a) -> Board -> Bool
checkIsMoveValid (x1,y1)(x2,y2) board
    | ((x2>=0) && (x2<=7) && (y2>=0) && (y2<=7) && (abs(x2-x1) <= 1) && (((get (x1,y1) board) == WS) || ((get (x1,y1) board) == BS))) = True
    | otherwise = False

deleteFromBoard :: Board -> Position -> Board
deleteFromBoard m (x,y) = updateBoard m E (x,y)

standardToQueen :: Figure -> Board -> Position -> Board
standardToQueen WS board (x,y) = updateBoard board WQ (x,y)
standardToQueen BS board (x,y) = updateBoard board BQ (x,y)