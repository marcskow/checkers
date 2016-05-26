import Data.Tree
import Data.Maybe

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

startingBoard = reverse [[ E, BS, E, BS, E, BS, E, BS], [ BS, E, BS, E, BS, E, BS, E ],
		 [ E, BS, E, BS, E, BS, E, BS], [ E, E, E, E, E, E, E, E ],
		 [ E, E, E, E, E, E, E, E], [ WS, E, WS, E, WS, E, WS, E ],
		 [ E, WS, E, WS, E, WS, E, WS ], [ WS, E, WS, E, WS, E, WS, E ]]

showFigure WS = "w"
showFigure BS = "b"
showFigure WQ = "W"
showFigure BQ = "B"
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

genWSMoves (a,b) = filter (\(x,y) -> and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b+f),(a-f,b+f)])

genBSMoves (a,b) = filter (\(x,y) -> and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b-f),(a-f,b-f)])

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

canWSHitLeft (x,y) board = ((get (left LEFT_UP 1 (x,y)) board) == BS) && ((get (left LEFT_UP 2 (x,y)) board) == E)
canWSHitRight (x,y) board = ((get (right RIGHT_UP 1 (x,y)) board) == BS) && ((get (right RIGHT_UP 2 (x,y)) board) == E)

buildHittingTree False (x,y) board = Nil
buildHittingTree t (x,y) board =
    let leftTree = (buildHittingTree (canWSHitLeft (x,y) board) (x-2,y+2) board)
        rightTree = (buildHittingTree (canWSHitRight (x,y) board) (x+2,y+2) board)
    in HittingNode (x,y) [ leftTree, rightTree ]

updateMatrix m x (c,r) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

checkIsMoveValid (x1,y1)(x2,y2) board
    | ((((get (x1,y1) board) == WS) || ((get (x1,y1) board) == BS)) && (x2>=0) && (x2<=7) && (y2>=0) && (y2<=7) && (abs(x2-x1) <= 1)) = True
    | otherwise = False

deleteFromBoard m (r,c) = updateMatrix m E (r,c)

move' board (x1,y1)(x2,y2) =
    if ((checkIsMoveValid (x1,y1)(x2,y2) board) == True) then
          (deleteFromBoard (updateMatrix board (get (x1,y1) board) (x2,y2)) (x1,y1))
       else board

  --  else ((checkIsMoveValid (x1,y1)(x2,y2)) == True) (updateMatrix board (get (x1,y1)) (x2,y2))



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