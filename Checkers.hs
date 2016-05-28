import qualified Data.Foldable as F
import Data.Monoid
import Data.List
import Data.Function
import Board
import Data.Ord

data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show
data Move = WSL | WQL | WSR | WQR | BSL | BSR | BQL | BQR deriving Eq
data Direction = LEFT_UP | LEFT_DOWN | RIGHT_UP | RIGHT_DOWN

treeToLoL Nil = []
treeToLoL (HittingNode a [ l, r ]) = [a] : merge (treeToLoL l) (treeToLoL r)

merge [] ys = ys
merge xs [] = xs
merge (x:xs)(y:ys) = (x ++ y) : merge xs ys

genMoves (a,b) board
    | ((get (a,b) board) == WS) = genWSMoves (a,b)
    | ((get (a,b) board) == BS) = genBSMoves (a,b)

genWSMoves (a,b) = filter (\(x,y) -> Prelude.and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b+f),(a-f,b+f)])

genBSMoves (a,b) = filter (\(x,y) -> Prelude.and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b-f),(a-f,b-f)])

filterNormalMoves [] _ = []
filterNormalMoves (x : xs) board =
    (filterOneMove x (get(x) board)) ++ (filterNormalMoves xs board)

filterOneMove x t
    | t == E = [x]
    | otherwise = []

move t u i (x,y)
    | t == WSL = leftSt WS (x,y)
    | t == WSR = rightSt WS (x,y)
    | t == BSL = leftSt BS (x,y)
    | t == BSR = rightSt BS (x,y)
    | t == WQL = nextField u i (x,y)
    | t == WQR = nextField u i (x,y)
    | t == BQL = nextField u i (x,y)
    | t == BQR = nextField u i (x,y)

leftSt' (x,y) board
    | ((get (x,y) board) == WS) = leftWS (x,y)
    | ((get (x,y) board) == BS) = leftBS (x,y)

-- Normale
leftWS (x,y) = leftSt WS (x,y)
rightWS (x,y) = rightSt WS (x,y)
leftBS (x,y) = leftSt BS (x,y)
rightBS (x,y) = rightSt BS (x,y)
leftSt WS (x,y) = (x-1,y+1)
leftSt BS (x,y) = (x-1,y-1)
rightSt WS (x,y) = (x+1,y+1)
rightSt BS (x,y) = (x+1,y+1)

-- Queensy
nextField LEFT_DOWN i (x,y) = (x-i,y-i)
nextField LEFT_UP i (x,y) = (x-i,y+i)
nextField RIGHT_DOWN i (x,y) = (x+i,y-i)
nextField RIGHT_UP i (x,y) = (x+i,y+i)

checkIsLeftEmpty t (x,y) board = ((get (leftSt t (x,y)) board) == E)
checkIsLeftOpponent t (x,y) board = ((get (leftSt t (x,y)) board) == (opposedFigure t))

{-
    For now only standard pawns, I must think about queens
-}
willBeInside (x,y) LEFT_UP = ((x-2>=0) && (y+2<=7))
willBeInside (x,y) LEFT_DOWN = ((x-2>=0) && (y-2>=0))
willBeInside (x,y) RIGHT_UP = ((x+2<=7) && (y+2<=7))
willBeInside (x,y) RIGHT_DOWN = ((x+2<=7) && (y-2>=0))

canHit WS dir (x,y) board = (willBeInside (x,y) dir) && ((get (nextField dir 1 (x,y)) board) == BS) && ((get (nextField dir 2 (x,y)) board) == E)
canHit BS dir (x,y) board = (willBeInside (x,y) dir) && ((get (nextField dir 1 (x,y)) board) == WS) && ((get (nextField dir 2 (x,y)) board) == E)
{-
canHit LEFT_UP (x,y) board =  ((x-2>=0) && (y+2<=7)) && ((get (nextField LEFT_UP 1 (x,y)) board) == (opposedFigure (get (x,y) board))) && ((get (nextField LEFT_UP 2 (x,y)) board) == E)
canHit LEFT_DOWN (x,y) board = ((x-2>=0) && (y-2<=7)) && ((get (nextField LEFT_DOWN 1 (x,y)) board) == (opposedFigure (get (x,y) board))) && ((get (nextField LEFT_DOWN 2 (x,y)) board) == E)
canHit RIGHT_UP (x,y) board = ((x+2<=7) && (y+2<=7)) && ((get (nextField RIGHT_UP 1 (x,y)) board) == (opposedFigure (get (x,y) board))) && ((get (nextField RIGHT_UP 2 (x,y)) board) == E)
canHit RIGHT_DOWN (x,y) board = ((x+2<=7) && (y-2>=0)) && ((get (nextField RIGHT_DOWN 1 (x,y)) board) == (opposedFigure (get (x,y) board))) && ((get (nextField RIGHT_UP 2 (x,y)) board) == E)
-}
-- updateBoard b3 E (2,4)

buildHittingTree w False (x,y) board = Nil
buildHittingTree w t (x,y) board =
    let leftTreeUp =    if (canHit w LEFT_UP (x,y) board) then (buildHittingTree w True (nextField LEFT_UP 2 (x,y)) (hittingMove board (x,y) (nextField LEFT_UP 2 (x,y))))
                        else Nil
        leftTreeDown =  if (canHit w LEFT_DOWN (x,y) board) then (buildHittingTree w True (nextField LEFT_DOWN 2 (x,y)) (hittingMove board (x,y) (nextField LEFT_DOWN 2 (x,y))))
                        else Nil
        rightTreeUp =   if (canHit w RIGHT_UP (x,y) board) then (buildHittingTree w True (nextField RIGHT_UP 2 (x,y)) (hittingMove board (x,y) (nextField RIGHT_UP 2 (x,y))))
                        else Nil
        rightTreeDown = if (canHit w RIGHT_DOWN (x,y) board) then (buildHittingTree w True (nextField RIGHT_DOWN 2 (x,y)) (hittingMove board (x,y) (nextField RIGHT_DOWN 2 (x,y))))
                        else Nil
    in HittingNode (x,y) [ leftTreeUp, leftTreeDown, rightTreeUp, rightTreeDown ]


--data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show, Eq

listtree' Nil = []
listtree' (HittingNode label [Nil,Nil,Nil,Nil]) = [[label]]
listtree' (HittingNode label xs) = map (label:) $ concat $ map listtree' xs
{-
size Nil = 0
size (HittingNode _ [ leftUpTree, leftDownTree, rightUpTree, rightDownTree ]) = 1 + max (max ((size leftUpTree) (size leftDownTree))) (max((size rightUpTree) (size rightDownTree))) -}
-- hittings tree = (size tree) - 1

maximumByM c (x:xs) = maximumByM' c xs [x]
  where maximumByM' _ [] acc = acc
        maximumByM' c (x:xs) acc@(a:_) = case x `c` a of
          LT -> maximumByM' c xs acc
          EQ -> maximumByM' c xs (x:acc)
          GT -> maximumByM' c xs [x]

getMaximumHittingPath tree = maximumByM (comparing length) (listtree' tree)

hittingMoves Nil = 0 

updateBoard m x (c,r) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

checkIsMoveValid (x1,y1)(x2,y2) board
    | ((x2>=0) && (x2<=7) && (y2>=0) && (y2<=7) && (abs(x2-x1) <= 1) && (((get (x1,y1) board) == WS) || ((get (x1,y1) board) == BS))) = True
    | otherwise = False

deleteFromBoard m (r,c) = updateBoard m E (r,c)

move' board (x1,y1)(x2,y2) =
    if ((checkIsMoveValid (x1,y1)(x2,y2) board) == True) then
          (deleteFromBoard (updateBoard board (get (x1,y1) board) (x2,y2)) (x1,y1))
       else board

hittingMove board (x1,y1)(x2,y2) = (deleteFromBoard (deleteFromBoard (updateBoard board (get (x1,y1) board) (x2,y2)) (x1,y1)) (((x1+x2) `div` 2),((y1+y2) `div` 2)))

  --  else ((checkIsMoveValid (x1,y1)(x2,y2)) == True) (updateBoard board (get (x1,y1)) (x2,y2))

treeToList (HittingNode n a) = n : Prelude.concat (map treeToList a)


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
