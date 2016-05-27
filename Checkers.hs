import Data.Tree
import Data.Maybe
import qualified Data.Foldable as F
import Data.Monoid

data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show
data Figure = WS | BS | WQ | BQ | E deriving Show
data Field = Empty | Pawn
data Move = WSL | WQL | WSR | WQR | BSL | BSR | BQL | BQR deriving Eq
data Direction = LEFT_UP | LEFT_DOWN | RIGHT_UP | RIGHT_DOWN

opposedFigure :: Figure -> Figure
opposedFigure f
    | f == WS = BS
    | f == BS = WS
    | f == BQ = WQ
    | f == WQ = BQ
    | f == E = E

instance Eq Figure where
    WS == WS = True
    BS == BS = True
    WQ == WQ = True
    BQ == BQ = True
    E == E = True
    _ == _ = False

instance F.Foldable HittingTree where
   foldMap f Nil = mempty
   foldMap f (HittingNode x [Nil,Nil]) = f x
   foldMap f (HittingNode x [l,r]) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

treeToLoL Nil = []
treeToLoL (HittingNode a [ l, r ]) = [a] : merge (treeToLoL l) (treeToLoL r)

merge [] ys = ys
merge xs [] = xs
merge (x:xs)(y:ys) = (x ++ y) : merge xs ys

startingBoard = reverse [[ E, BS, E, BS, E, BS, E, BS], [ BS, E, BS, E, BS, E, BS, E ],
		 [ E, BS, E, BS, E, BS, E, BS], [ E, E, E, E, E, E, E, E ],
		 [ E, E, E, E, E, E, E, E], [ WS, E, WS, E, WS, E, WS, E ],
		 [ E, WS, E, WS, E, WS, E, WS ], [ WS, E, WS, E, WS, E, WS, E ]]
--26C0 26C1 26C2 26C3 9920 9921 9922 9923
showFigure WS = "\9922"
showFigure BS = "\9920"
showFigure WQ = "\9923"
showFigure BQ = "\9921"
showFigure E = "."

showRow [] = " \n"
showRow (x : xs) = (showFigure x) ++ " " ++ (showRow xs)

showBoard [] = ""
showBoard (x : xs) = (showRow x) ++ (showBoard xs)

boardInRows board = lines (showBoard board)

showDescription [][] = "  0 1 2 3 4 5 6 7 \n"
showDescription (x : xs) (y : ys) = x ++ y ++ "\n" ++ (showDescription xs ys)

rowsIndex = reverse [ "7 ", "6 ", "5 ", "4 ", "3 ", "2 ", "1 ","0 " ]

sh board= putStr("  0 1 2 3 4 5 6 7 \n" ++ (showDescription rowsIndex (boardInRows board)))


{-

Accessing board

-}

getRow x board = board !! x

get (x,y) board = (board !! y) !! x

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
    | t == WQL = left u i (x,y)
    | t == WQR = right u i (x,y)
    | t == BQL = left u i (x,y)
    | t == BQR = right u i (x,y)

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
left LEFT_DOWN i (x,y) = (x-i,y-i)
left LEFT_UP i (x,y) = (x-i,y+i)
right RIGHT_DOWN i (x,y) = (x+i,y-i)
right RIGHT_UP i (x,y) = (x+i,y+i)

checkIsLeftEmpty t (x,y) board = ((get (leftSt t (x,y)) board) == E)
checkIsLeftOpponent t (x,y) board = ((get (leftSt t (x,y)) board) == (opposedFigure t))

canWSHitLeft (x,y) board =  ((x-2>=0) && (y+2<=7)) && ((get (left LEFT_UP 1 (x,y)) board) == BS) && ((get (left LEFT_UP 2 (x,y)) board) == E)
canWSHitRight (x,y) board = ((x-2>=0) && (y+2<=7)) && ((get (right RIGHT_UP 1 (x,y)) board) == BS) && ((get (right RIGHT_UP 2 (x,y)) board) == E)

buildHittingTree False (x,y) board = Nil
buildHittingTree t (x,y) board =
    let leftTree = (buildHittingTree (canWSHitLeft (x,y) board) (x-2,y+2) board)
        rightTree = (buildHittingTree (canWSHitRight (x,y) board) (x+2,y+2) board)
    in HittingNode (x,y) [ leftTree, rightTree ]


--data HittingTree a = Nil | HittingNode a [ HittingTree a ] deriving Show, Eq

listtree' Nil = []
listtree' (HittingNode label [Nil,Nil]) = [[label]]
listtree' (HittingNode label xs) = map (label:) $ concat $ map listtree' xs

size Nil = 0
size (HittingNode _ [ leftTree, rightTree ]) = 1 + max (size leftTree) (size rightTree)
hittings tree = (size tree) - 1


hittingMoves Nil = 0 

updateMatrix m x (c,r) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

checkIsMoveValid (x1,y1)(x2,y2) board
    | ((x2>=0) && (x2<=7) && (y2>=0) && (y2<=7) && (abs(x2-x1) <= 1) && (((get (x1,y1) board) == WS) || ((get (x1,y1) board) == BS))) = True
    | otherwise = False

deleteFromBoard m (r,c) = updateMatrix m E (r,c)

move' board (x1,y1)(x2,y2) =
    if ((checkIsMoveValid (x1,y1)(x2,y2) board) == True) then
          (deleteFromBoard (updateMatrix board (get (x1,y1) board) (x2,y2)) (x1,y1))
       else board

  --  else ((checkIsMoveValid (x1,y1)(x2,y2)) == True) (updateMatrix board (get (x1,y1)) (x2,y2))

treeToList (HittingNode n a) = n : Prelude.concat (map treeToList a)

--pathsToNode x (HittingNode y ns) = [[x] | x == y] ++ map (y:) (pathsToNode x =<< ns)

--listtree (HittingNode label [Nil,Nil]) = [[label]]
--listtree (HittingNode label xs) = map (label:) $ concat $ map listtree xs

{-
let b1 = (move' startingBoard (5,5)(4,4))
let b2 = (move' b1 (4,4)(3,3))
let b1 = (move' b2 (1,5)(0,4))
let b2 = (move' b1 (0,4)(1,3))
let b1 = (move' b2 (4,6)(5,5))
let tree = buildHittingTree True (2,2) b1
let tree2 = buildHittingTree True (4,2) b1

hittings tree
hittings tree2
-}

{-
showRealDescription [][] = "  12345678 \n"
showRealDescription (x : xs) (y : ys) = x ++ y ++ "\n" ++ (showDescription xs ys)
rowsRealIndex = reverse [ "8 ", "7 ", "6 ", "5 ", "4 ", "3 ", "2 ", "1 " ]
showRealBoard' = putStr("  12345678\n" ++ (showRealDescription rowsIndex boardInRows))
realBoardInRows = lines (showBoard board)
-}
{-
updateBoard' board (x1,y1)(x2,y2) =
    case get (x1,y1) of
        Just figure -> updateMatrix
    Just figure -> updateMatrix

updateBoard :: Board -> Move -> Board
updateBoard board (RegularMove from to) =
  case Map.lookup from board of
    Just figure -> Map.insert to figure . Map.delete from $ board
    _ -> board
    -}