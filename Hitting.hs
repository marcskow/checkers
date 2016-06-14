{- Autor: Marcin Skowron, Informatyka II rok -}
module Hitting ( HittingTree(..)
    , buildHittingTree, buildHittingQueenTree
    , buildPaths, getMaximumHittingPath
) where

import Data.Function
import Data.Ord
import Move
import Board

data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show

{-
    This is function responsible for building hittings tree from given field. It checks if a pawn can make hit in
    the direction and then builds subtree from next field (field on which a pawn stands after hit).
    This function builds hittings tree only for standards pawns.
-}
buildHittingTree :: Figure -> Bool -> Position -> Board -> HittingTree Position
buildHittingTree w False (x,y) board = Nil
buildHittingTree w t (x,y) board =
    let next dir = (buildHittingTree w True (nextField dir 2 (x,y)) (move board (Hit (x,y) (nextField dir 2 (x,y)))))
        leftTreeUp =    if (canHit w LEFT_UP (x,y) board) then next LEFT_UP else Nil
        leftTreeDown =  if (canHit w LEFT_DOWN (x,y) board) then next LEFT_DOWN else Nil
        rightTreeUp =   if (canHit w RIGHT_UP (x,y) board) then next RIGHT_UP else Nil
        rightTreeDown = if (canHit w RIGHT_DOWN (x,y) board) then next RIGHT_DOWN else Nil
    in HittingNode (x,y) [ leftTreeUp, leftTreeDown, rightTreeUp, rightTreeDown ]

{-
    This is the same function as buildHittingTree but for queens.
-}
buildHittingQueenTree :: Figure -> Bool -> Position -> Board -> HittingTree Position
buildHittingQueenTree w False (x,y) board = Nil
buildHittingQueenTree w t (x,y) board =
    let firstQueenHit dir = (findFirstQueenHit w dir (x,y) board)
        next dir = (buildHittingQueenTree w True (head(firstQueenHit dir)) (move board (Hit (x,y) (head(firstQueenHit dir)))))
        leftTreeUp =   if ((firstQueenHit LEFT_UP) /= []) then (next LEFT_UP) else Nil
        leftTreeDown = if ((firstQueenHit LEFT_DOWN) /= []) then (next LEFT_DOWN) else Nil
        rightTreeUp =  if ((firstQueenHit RIGHT_UP) /= []) then (next RIGHT_UP) else Nil
        rightTreeDown = if ((firstQueenHit RIGHT_DOWN) /= []) then (next RIGHT_DOWN) else Nil
    in HittingNode (x,y) [ leftTreeUp, leftTreeDown, rightTreeUp, rightTreeDown ]

{-
    Check is opponent on the next field to the given position and is the field after next empty.
-}
canHit :: Figure -> Direction -> Position -> Board -> Bool
canHit WS dir (x,y) board = (isInside 2 (x,y) dir) && ((getColor((get (nextField dir 1 (x,y)) board))) == Black) && ((get (nextField dir 2 (x,y)) board) == E)
canHit BS dir (x,y) board = (isInside 2 (x,y) dir) && ((getColor((get (nextField dir 1 (x,y)) board))) == White) && ((get (nextField dir 2 (x,y)) board) == E)

{-
    If there are two pawns on the queen way, it cannot make hit (e.g. can't hit when: (diagonal) WQ E BS BS E BS E, or: WQ WS WS E BS E)
-}
blockingWay :: (Num a, Eq a) => a -> Position -> Direction -> Board -> Bool
blockingWay 1 (x,y) dir board = False
blockingWay f (x,y) dir board = (length ([ (a+1) | a <- [1..7], (isInside (a+1) (x,y) dir) && (((get (nextField dir a (x,y)) board) == E) == False) && (((get (nextField dir (a+1) (x,y)) board) == E) == False)])) /= 0

{-
    Looking for queen hit
-}
findFirstQueenHitStep :: Figure -> Direction -> Position -> Board -> [Int]
findFirstQueenHitStep w dir (x,y) board = [ f+1 | f <- [1..7], ((blockingWay f (x,y) dir board) == False ) && (isInside (f+1) (x,y) dir)
    && ((getColor (get (nextField dir f (x,y)) board)) == opposedColor (getColor (get (x,y) board))) && ((get (nextField dir (f+1) (x,y)) board) == E)]

findFirstQueenHit :: Figure -> Direction -> Position -> Board -> [Position]
findFirstQueenHit w dir (x,y) board
    | ((findFirstQueenHitStep w dir (x,y) board) == []) = []
    | otherwise = [(nextField dir (head (findFirstQueenHitStep w dir (x,y) board)) (x,y))]

{-
    Helper function to map hitting tree into list
-}
listtree :: HittingTree a -> [[a]]
listtree Nil = []
listtree (HittingNode label [Nil,Nil,Nil,Nil]) = [[label]]
listtree (HittingNode label xs) = map (label:) $ concat $ map listtree xs

{-
    This function returns maximum value using comparing function. If there are more then one max value it returns all of max values.
    I use this function to find maximum hitting way.
-}
maximumBy :: (t -> t -> Ordering) -> [t] -> [t]
maximumBy c (x:xs) = maximumBy' c xs [x]
  where maximumBy' _ [] acc = acc
        maximumBy' c (x:xs) acc@(a:_) = case x `c` a of
          LT -> maximumBy' c xs acc
          EQ -> maximumBy' c xs (x:acc)
          GT -> maximumBy' c xs [x]

getMaximumHittingPath :: HittingTree a -> [[a]]
getMaximumHittingPath (HittingNode label []) = []
getMaximumHittingPath tree = maximumBy (comparing length) (listtree tree)

buildHittingMovesFromPath :: [Position] -> Move
buildHittingMovesFromPath [] = HittingSequence []
buildHittingMovesFromPath path = HittingSequence [ Hit (path !! x) (path !! (x+1)) | x <- [0..(length(path)-2)]]

buildPaths :: [[Position]] -> [Move]
buildPaths [] = []
buildPaths (path : paths) = [buildHittingMovesFromPath path] ++ buildPaths paths
