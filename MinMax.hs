module MinMax ( minmax, build, genAllPossibleMoves, generatePlayTree, getMaximumPath, getMaxLeaf, estimateMap,count ) where

import Board
import Move
import Hitting
import Generate
import Utils
import Data.Tree

data EstimatedMap = Estimated [[Figure]] Int deriving Show

buildSubForest :: Color -> Color -> Int -> [Board] -> [Tree EstimatedMap]
buildSubForest myColor color depth [] = []
buildSubForest myColor color depth (map : maps) = [Node (Estimated map (estimateMap myColor map)) [(generatePlayTree myColor (opposedColor color) (depth - 1) map)]] ++ (buildSubForest myColor color depth maps)

listtree' :: Tree a -> [[a]]
listtree' (Node label []) = [[label]]
listtree' (Node label xs) = map (label:) $ concat $ map listtree' xs

{-
    gdyby mieć możliwe ruchy dla danej mapy, a następnie ocenić każdy ruch za pomocą minmaxa..
-}

generatePlayTree :: Color -> Color -> Int -> Board -> Tree EstimatedMap
generatePlayTree myColor color 0 actualmap = Node (Estimated actualmap (estimateMap myColor actualmap)) []
generatePlayTree myColor color depth actualmap =
    let moves = (genAllPossibleMoves playableArea color actualmap)
        maps = (genAllPossibleMaps moves color actualmap)
        subforest = (buildSubForest myColor color depth maps)
        estimate = estimateMap myColor actualmap
        in Node (Estimated actualmap estimate) subforest

minmax :: Color -> Color -> Int -> Board -> Move
minmax myColor color depth actualmap = getMaxMove movesAndWeights where movesAndWeights = build (genAllPossibleMoves playableArea color actualmap) myColor color depth actualmap

getMaxMove :: Ord a => [(Move, a)] -> Move
getMaxMove [] = Step (0,0)(0,0)
getMaxMove (x:xs) =
    let (move,weight) = x
    in if(weight == (maximum (buildOnlyWeights (x:xs)))) then move else (getMaxMove xs)

build :: [Move] -> Color -> Color -> Int -> Board -> [(Move,Int)]
build [] myColor starting depth actual = []
build (m:moves) myColor starting depth actual = [(m,getMaximumPath myColor starting depth actual m)] ++ build moves myColor starting depth actual

buildOnlyWeights :: [(t1,t)] -> [t]
buildOnlyWeights [] = []
buildOnlyWeights (x:xs) =
    let (a,b) = x
    in [b] ++ buildOnlyWeights xs

getMaximumPath :: Color -> Color -> Int -> Board -> Move -> Int
getMaximumPath cl stCl d map m = getMaxLeaf leaves where leaves = getLeaves tree where tree = generatePlayTree cl (opposedColor stCl) d (move map m)

getLeaves :: Tree t -> [t]
getLeaves (Node current []) = [current]
getLeaves (Node current children) = (children >>= getLeaves)

getMaxLeaf :: [EstimatedMap] -> Int
getMaxLeaf [] = (-50000)
getMaxLeaf leaves =
    let weights = buildOnlyLeavesWeights leaves
    in maximum weights

buildOnlyLeavesWeights :: [EstimatedMap] -> [Int]
buildOnlyLeavesWeights [] = []
buildOnlyLeavesWeights (x:xs) =
    let Estimated a b = x
    in [b] ++ buildOnlyLeavesWeights xs

genAllPossibleMoves :: [Position] -> Color -> Board -> [Move]
genAllPossibleMoves [] c actual = []
genAllPossibleMoves (f : fields) c actual = filterMoves(genAllPossibleMoves' (f : fields) c actual)

isMoveHitting :: Move -> Bool
isMoveHitting (Step _ _) = False
isMoveHitting (HittingSequence a) = True

areHittingsInside :: [Move] -> Bool
areHittingsInside [] = False
areHittingsInside (m : moves) = (isMoveHitting m) || (areHittingsInside moves)

getOnlyHittings :: [Move] -> [Move]
getOnlyHittings [] = []
getOnlyHittings (m:moves) = (if(isMoveHitting m) then [m] else []) ++ getOnlyHittings moves

filterMoves :: [Move] -> [Move]
filterMoves [] = []
filterMoves (m : moves) = if(areHittingsInside (m:moves)) then getOnlyHittings (m:moves) else (m:moves)

genAllPossibleMoves' :: [Position] -> Color -> Board -> [Move]
genAllPossibleMoves' [] c actual = []
genAllPossibleMoves' (f : fields) c actual = let thisFieldMoves = if (is c f actual) then (genMoves f actual) else []
                    in thisFieldMoves ++ (genAllPossibleMoves' fields c actual)

genAllPossibleMaps :: [Move] -> t -> Board -> [Board]
genAllPossibleMaps [] c actual = []
genAllPossibleMaps (m : moves) c actual = [move actual m] ++ (genAllPossibleMaps moves c actual)

estimateMap :: Color -> Board -> Int
estimateMap myColor map
    | myColor == White = sub (add (multi 3 $ count WS playableArea map) (multi 7 $ count WQ playableArea map)) (add (multi 3 $ count BS playableArea map) (multi 7 $ count BQ playableArea map))
    | myColor == Black = sub (add (multi 3 $ count BS playableArea map) (multi 7 $ count BQ playableArea map)) (add (multi 3 $ count WS playableArea map) (multi 7 $ count WQ playableArea map))


count :: Figure -> [Position] -> Board -> Int
count t [] board = 0
count t (f : fields) board = (if((get f board) == t) then 1 else 0) + count t fields board