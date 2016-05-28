import qualified Data.Foldable as F
import Data.Monoid
import Data.List
import Data.Function
import Board
import Move
import Data.Ord

data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show
data Direction = LEFT_UP | LEFT_DOWN | RIGHT_UP | RIGHT_DOWN

merge :: [[a]] -> [[a]] -> [[a]]
merge [] ys = ys
merge xs [] = xs
merge (x:xs)(y:ys) = (x ++ y) : merge xs ys

genMoves :: Position -> Board -> [Position]
genMoves (a,b) board
    | ((get (a,b) board) == WS) = genWSMoves (a,b)
    | ((get (a,b) board) == BS) = genBSMoves (a,b)

genWSMoves :: (Ord a, Num a, Enum a) => (a,a) -> [(a,a)]
genWSMoves (a,b) = filter (\(x,y) -> Prelude.and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b+f),(a-f,b+f)])
genBSMoves (a,b) = filter (\(x,y) -> Prelude.and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b-f),(a-f,b-f)])

filterNormalMoves :: [Position] -> Board -> [Position]
filterNormalMoves [] _ = []
filterNormalMoves (x : xs) board =
    (filterOneMove x (get(x) board)) ++ (filterNormalMoves xs board)

filterOneMove :: t -> Figure -> [t]
filterOneMove x t
    | t == E = [x]
    | otherwise = []

nextField :: Num t => Direction -> t -> (t,t) -> (t,t)
nextField LEFT_DOWN i (x,y) = (x-i,y-i)
nextField LEFT_UP i (x,y) = (x-i,y+i)
nextField RIGHT_DOWN i (x,y) = (x+i,y-i)
nextField RIGHT_UP i (x,y) = (x+i,y+i)

{-
    For now only standard pawns, I must think about queens
-}
willBeInside :: (Ord a1, Ord a, Num a1, Num a) => (a, a1) -> Direction -> Bool
willBeInside (x,y) LEFT_UP = ((x-2>=0) && (y+2<=7))
willBeInside (x,y) LEFT_DOWN = ((x-2>=0) && (y-2>=0))
willBeInside (x,y) RIGHT_UP = ((x+2<=7) && (y+2<=7))
willBeInside (x,y) RIGHT_DOWN = ((x+2<=7) && (y-2>=0))

canHit :: Figure -> Direction -> Position -> Board -> Bool
canHit WS dir (x,y) board = (willBeInside (x,y) dir) && ((get (nextField dir 1 (x,y)) board) == BS) && ((get (nextField dir 2 (x,y)) board) == E)
canHit BS dir (x,y) board = (willBeInside (x,y) dir) && ((get (nextField dir 1 (x,y)) board) == WS) && ((get (nextField dir 2 (x,y)) board) == E)

buildHittingTree :: Figure -> Bool -> Position -> Board -> HittingTree Position
buildHittingTree w False (x,y) board = Nil
buildHittingTree w t (x,y) board =
    let leftTreeUp =    if (canHit w LEFT_UP (x,y) board) then (buildHittingTree w True (nextField LEFT_UP 2 (x,y)) (move board (Hit (x,y) (nextField LEFT_UP 2 (x,y)))))
                        else Nil
        leftTreeDown =  if (canHit w LEFT_DOWN (x,y) board) then (buildHittingTree w True (nextField LEFT_DOWN 2 (x,y)) (move board (Hit (x,y) (nextField LEFT_DOWN 2 (x,y)))))
                        else Nil
        rightTreeUp =   if (canHit w RIGHT_UP (x,y) board) then (buildHittingTree w True (nextField RIGHT_UP 2 (x,y)) (move board (Hit (x,y) (nextField RIGHT_UP 2 (x,y)))))
                        else Nil
        rightTreeDown = if (canHit w RIGHT_DOWN (x,y) board) then (buildHittingTree w True (nextField RIGHT_DOWN 2 (x,y)) (move board (Hit (x,y) (nextField RIGHT_DOWN 2 (x,y)))))
                        else Nil
    in HittingNode (x,y) [ leftTreeUp, leftTreeDown, rightTreeUp, rightTreeDown ]

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
getMaximumHittingPath tree = maximumByM (comparing length) (listtree tree)


{-
let b1 = (move' startingBoard (5,5)(4,4))
let b2 = (move' b1 (4,4)(3,3))
let b1 = (move' b2 (1,5)(0,4))
let b2 = (move' b1 (0,4)(1,3))
let b1 = (move' b2 (4,6)(5,5))
let b2 = (move' b1 (0,6)(1,5))
let tree3 = buildHittingTree WS True (4,2) b2

hittings tree
hittings tree2
-}
