module MinMax (
      opposedColor
    , Color(..)
    , buildSubForest
    , listtree'
    , generatePlayTree
    , playableArea
    , is
    , genAllPossibleMoves
    , genAllPossibleMoves'
    , genAllPossibleMaps
    , count
    ,multi
    , tree
    , getLeaves
    ,estimateMap
    , getMapFromEstimated
    , getMaxLeaf
    , buildOnlyLeavesWeights
    , minmax
    , getMaxMove
    , buildOnlyWeights
    , build
    , isMoveHitting
    , getMaximumPath
    , filterMoves
    , areHittingsInside
    , EstimatedMap(..)
) where

import Board
import Move
import Generate
import Hitting
import Data.Tree


data Color = White | Black
data EstimatedMap = Estimated [[Figure]] Int deriving Show

instance Eq Color where
    White == White = True
    Black == Black = True
    _ == _ = False

opposedColor color
    | color == White = Black
    | color == Black = White

buildSubForest :: Color -> Color -> Int -> [Board] -> [Tree EstimatedMap]
buildSubForest myColor color depth [] = []
buildSubForest myColor color depth (map : maps) = [Node (Estimated map (estimateMap myColor map)) [(generatePlayTree myColor (opposedColor color) (depth - 1) map)]] ++ (buildSubForest myColor color depth maps)


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


danke = 5



minmax myColor color depth actualmap = getMaxMove movesAndWeights where movesAndWeights = build (genAllPossibleMoves playableArea color actualmap) myColor color depth actualmap

-- getMaxMove [] = Step (0,0) (0,0)
getMaxMove (x:xs) =
    let (move,weight) = x
    in if(weight == (maximum (buildOnlyWeights (x:xs)))) then move else (getMaxMove xs)

build [] myColor starting depth actual = []
build (m:moves) myColor starting depth actual = [(m,getMaximumPath myColor starting depth actual m)] ++ build moves myColor starting depth actual

buildOnlyWeights [] = []
buildOnlyWeights (x:xs) =
    let (a,b) = x
    in [b] ++ buildOnlyWeights xs

-- coor startcolor depth map move
getMaximumPath cl stCl d map m = getMaxLeaf leaves where leaves = getLeaves tree where tree = generatePlayTree cl (opposedColor stCl) d (move map m)


getLeaves (Node current []) = [current]
getLeaves (Node current children) = (children >>= getLeaves)

getMaxLeaf [] = (-50000)
getMaxLeaf leaves =
    let weights = buildOnlyLeavesWeights leaves
    in maximum weights

buildOnlyLeavesWeights [] = []
buildOnlyLeavesWeights (x:xs) =
    let Estimated a b = x
    in [b] ++ buildOnlyLeavesWeights xs

getMapFromEstimated (Estimated x y) = x

tree = Node 5 [Node 10[], Node 20[], Node 30[]]

playableArea :: [Position]
playableArea = [(0,0),(2,0),(4,0),(6,0),(1,1),(3,1),(5,1),(7,1),
                (0,2),(2,2),(4,2),(6,2),(1,3),(3,3),(5,3),(7,3),
                (0,4),(2,4),(4,4),(6,4),(1,5),(3,5),(5,5),(7,5),
                (0,6),(2,6),(4,6),(6,6),(1,7),(3,7),(5,7),(7,7)]

is White (a,b) board = ((get (a,b) board) == WS) || ((get (a,b) board) == WQ)
is Black (a,b) board = ((get (a,b) board) == BS) || ((get (a,b) board) == BQ)

genAllPossibleMoves [] c actual = []
genAllPossibleMoves (f : fields) c actual = filterMoves(genAllPossibleMoves' (f : fields) c actual)

isMoveHitting (Step _ _) = False
isMoveHitting (HittingSequence a) = True

areHittingsInside [] = False
areHittingsInside (m : moves) = (isMoveHitting m) || (areHittingsInside moves)

getOnlyHittings [] = []
getOnlyHittings (m:moves) = (if(isMoveHitting m) then [m] else []) ++ getOnlyHittings moves

filterMoves (m : moves) = if(areHittingsInside (m:moves)) then getOnlyHittings (m:moves) else (m:moves)

genAllPossibleMoves' [] c actual = []
genAllPossibleMoves' (f : fields) c actual = let thisFieldMoves = if (is c f actual) then (genMoves f actual) else []
                                            in thisFieldMoves ++ (genAllPossibleMoves' fields c actual)
genAllPossibleMaps [] c actual = []
genAllPossibleMaps (m : moves) c actual = [(move actual m)] ++ (genAllPossibleMaps moves c actual)

estimateMap :: Color -> Board -> Int
estimateMap myColor map
    | (myColor == White) = sub (add (multi (count WS playableArea map) 3) (multi (count WQ playableArea map) 7)) (add (multi (count BS playableArea map) 3) (multi (count BQ playableArea map) 7))
    | (myColor == Black) = sub (add (multi (count BS playableArea map) 3) (multi (count BQ playableArea map) 7)) (add (multi (count WS playableArea map) 3) (multi (count WQ playableArea map) 7))

add :: Int -> Int -> Int
add x y = x + y

sub :: Int -> Int -> Int
sub x y = x - y

multi :: Int -> Int -> Int
multi x y = x * y

count :: Figure -> [Position] -> Board -> Int
count t [] board = 0
count t (f : fields) board = (if((get f board) == t) then 1 else 0) + (count t fields board)