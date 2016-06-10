{- Autor: Marcin Skowron, Informatyka II rok -}
module Move (  Move(..), Direction(..)
             , move, updateBoard, standardToQueen
             , nextField, previousField, isInside
             , getDirection, moveEstimation
             ) where

import Board

data Move = Step Position Position | Hit Position Position | HittingSequence [ Move ] deriving (Show, Eq)
data Direction = LEFT_UP | LEFT_DOWN | RIGHT_UP | RIGHT_DOWN

{-
    This is function responsible for moves. There are three types of move:
    - Step Position Position (just move pawn from one position to other)
    - Hit Position Position (never used directly)
    - HittingSequence [ Move ] containing only Hits, one hit is also HittingSequence with one hit inside
-}
move :: Board -> Move -> Board
move board (Hit (x1,y1) (x2,y2)) =
    let board' = let board'' = updateBoard board (get (x1,y1) board) (x2,y2) in (deleteFromBoard board'' (x1,y1))
    in (deleteFromBoard board' (previousField((getDirection (x1,y1)(x2,y2))) 1 (x2,y2)))
move board (Step (x1,y1) (x2,y2))
    | ((checkIsMoveValid (x1,y1)(x2,y2) board) == True) =
        let board' = (updateBoard board (get (x1,y1) board) (x2,y2))
        in if(isQueenField board (get (x1,y1) board) y2) then standardToQueen (get (x1,y1) board) (deleteFromBoard board' (x1,y1)) (x2,y2)
           else (deleteFromBoard board' (x1,y1))
    | otherwise = board
move board (HittingSequence []) = board
move board (HittingSequence (x:[])) =
    let Hit(x1,y1)(x2,y2) = x
    in if (isQueenField board (get (x1,y1) board) y2) then standardToQueen (get (x1,y1) board) (move board x) (x2,y2)
       else move board x
move board (HittingSequence (x:xs)) =
    let board' = move board (x)
    in move board' (HittingSequence xs)

updateBoard :: Board -> Figure -> Position -> Board
updateBoard m x (c,r) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

{-
    This function is not the main function for moves validation, the move choosen by user is validated
    after user interaction, by checking if possible moves (from moves generator) contains the move
    choosen by user. But this function has been implemented much earlier, so let's leave it here.
-}
checkIsMoveValid :: Position -> Position -> Board -> Bool
checkIsMoveValid (x1,y1)(x2,y2) board
    | ((x2>=0) && (x2<=7) && (y2>=0) && (y2<=7) && (abs(x2-x1) <= 1) && (((get (x1,y1) board) == WS) || ((get (x1,y1) board) == BS))) = True
    | ((x2>=0) && (x2<=7) && (y2>=0) && (y2<=7) && (((get (x1,y1) board) == WQ) || ((get (x1,y1) board) == BQ))) = True
    | otherwise = False

nextField :: Direction -> Int -> Position -> Position
nextField LEFT_DOWN i (x,y) = (x-i,y-i)
nextField LEFT_UP i (x,y) = (x-i,y+i)
nextField RIGHT_DOWN i (x,y) = (x+i,y-i)
nextField RIGHT_UP i (x,y) = (x+i,y+i)

previousField :: Direction -> Int -> Position -> Position
previousField RIGHT_UP i (x,y) = (x-i,y-i)
previousField RIGHT_DOWN i (x,y) = (x-i,y+i)
previousField LEFT_UP i (x,y) = (x+i,y-i)
previousField LEFT_DOWN i (x,y) = (x+i,y+i)

isInside :: Int -> Position -> Direction -> Bool
isInside f (x,y) LEFT_UP = ((x-f>=0) && (y+f<=7))
isInside f (x,y) LEFT_DOWN = ((x-f>=0) && (y-f>=0))
isInside f (x,y) RIGHT_UP = ((x+f<=7) && (y+f<=7))
isInside f (x,y) RIGHT_DOWN = ((x+f<=7) && (y-f>=0))

getDirection :: Position -> Position -> Direction
getDirection (x1, y1)(x2, y2)
    | (x2 < x1 && y2 > y1) = LEFT_UP
    | (x2 < x1 && y2 < y1) = LEFT_DOWN
    | (x2 > x1 && y2 > y1) = RIGHT_UP
    | (x2 > x1 && y2 < y1) = RIGHT_DOWN

isQueenField :: Board -> Figure -> Int -> Bool
isQueenField board WS 7 = True
isQueenField board WS _ = False
isQueenField board BS 0 = True
isQueenField board BS _ = False
isQueenField board _ _ = False

deleteFromBoard :: Board -> Position -> Board
deleteFromBoard m (x,y) = updateBoard m E (x,y)

standardToQueen :: Figure -> Board -> Position -> Board
standardToQueen WS board (x,y) = updateBoard board WQ (x,y)
standardToQueen BS board (x,y) = updateBoard board BQ (x,y)

moveEstimation :: Move -> Int
moveEstimation ( Step _ _ ) = 0
moveEstimation ( Hit _ _ ) = 3
moveEstimation ( HittingSequence [] ) = 0
moveEstimation ( HittingSequence (x:xs) ) = (moveEstimation x) + (moveEstimation (HittingSequence xs))