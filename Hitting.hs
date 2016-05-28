module Hitting ( HittingTree(..)
    , buildHittingTree
    , buildHittingQueenTree
    , canHit
    , blockingWay
    , findFirstQueenHitStep
    , findFirstQueenHit
    , listtree
    , maximumByM
    , getMaximumHittingPath
    , chooseTheBestMaximumHittingPath
    , filterToNotPrintCurrentField
    , buildHittingMovesFromPath
    , buildPaths
) where

import Data.Function
import Data.Ord
import Move
import Board

data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show


buildHittingTree :: Figure -> Bool -> Position -> Board -> HittingTree Position
buildHittingTree w False (x,y) board = Nil
buildHittingTree w t (x,y) board =
    let next dir = (buildHittingTree w True (nextField dir 2 (x,y)) (move board (Hit (x,y) (nextField dir 2 (x,y)))))
        leftTreeUp =    if (canHit w LEFT_UP (x,y) board) then next LEFT_UP else Nil
        leftTreeDown =  if (canHit w LEFT_DOWN (x,y) board) then next LEFT_DOWN else Nil
        rightTreeUp =   if (canHit w RIGHT_UP (x,y) board) then next RIGHT_UP else Nil
        rightTreeDown = if (canHit w RIGHT_DOWN (x,y) board) then next RIGHT_DOWN else Nil
    in HittingNode (x,y) [ leftTreeUp, leftTreeDown, rightTreeUp, rightTreeDown ]

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

canHit :: Figure -> Direction -> Position -> Board -> Bool
canHit WS dir (x,y) board = (isInside 2 (x,y) dir) && ((get (nextField dir 1 (x,y)) board) == BS) && ((get (nextField dir 2 (x,y)) board) == E)
canHit BS dir (x,y) board = (isInside 2 (x,y) dir) && ((get (nextField dir 1 (x,y)) board) == WS) && ((get (nextField dir 2 (x,y)) board) == E)

-- TODO
blockingWay :: (Num a, Eq a) => a -> Position -> Direction -> Board -> Bool
blockingWay 1 (x,y) dir board = False
blockingWay f (x,y) dir board = (length ([ (a+1) | a <- [1..7], (isInside (a+1) (x,y) dir) && ((get (nextField dir a (x,y)) board) == BS) && ((get (nextField dir (a+1) (x,y)) board) == BS)])) /= 0

findFirstQueenHitStep :: Figure -> Direction -> Position -> Board -> [Int]
findFirstQueenHitStep WQ dir (x,y) board = [ f+1 | f <- [1..7], ((blockingWay f (x,y) dir board) == False ) && (isInside (f+1) (x,y) dir) && ((get (nextField dir f (x,y)) board) == BS) && ((get (nextField dir (f+1) (x,y)) board) == E)]

findFirstQueenHit :: Figure -> Direction -> Position -> Board -> [Position]
findFirstQueenHit WQ dir (x,y) board
    | ((findFirstQueenHitStep WQ dir (x,y) board) == []) = []
    | otherwise = [(nextField dir (head (findFirstQueenHitStep WQ dir (x,y) board)) (x,y))]

listtree :: HittingTree a -> [[a]]
listtree Nil = []
listtree (HittingNode label [Nil,Nil,Nil,Nil]) = [[label]]
listtree (HittingNode label xs) = map (label:) $ concat $ map listtree xs


maximumByM :: (t -> t -> Ordering) -> [t] -> [t]
maximumByM c (x:xs) = maximumByM' c xs [x]
  where maximumByM' _ [] acc = acc
        maximumByM' c (x:xs) acc@(a:_) = case x `c` a of
          LT -> maximumByM' c xs acc
          EQ -> maximumByM' c xs (x:acc)
          GT -> maximumByM' c xs [x]

getMaximumHittingPath :: HittingTree a -> [[a]]
getMaximumHittingPath (HittingNode label []) = []
getMaximumHittingPath tree = maximumByM (comparing length) (listtree tree)

{-
    Magicznie wybierz najlepsza
-} -- TODO !!!!!!!!!!!!!!!!!!!
chooseTheBestMaximumHittingPath :: [[t]] -> [t]
chooseTheBestMaximumHittingPath path
    | length(path) == 0 = []
    | length(path) == 1 = head path
    | otherwise = head path --for now

filterToNotPrintCurrentField :: [t] -> [t]
filterToNotPrintCurrentField path
    | (length(path) == 1) = []
    | otherwise = path

buildHittingMovesFromPath :: [Position] -> Move
buildHittingMovesFromPath [] = HittingSequence []
buildHittingMovesFromPath path = HittingSequence [ Hit (path !! x) (path !! (x+1)) | x <- [0..(length(path)-2)]]

buildPaths :: [[Position]] -> [Move]
buildPaths [] = []
buildPaths (path : paths) = [buildHittingMovesFromPath path] ++ buildPaths paths
