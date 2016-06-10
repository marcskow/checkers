module Board ( opposedFigure, startingBoard, sh
			 , get, opposedColor, getColor, is
			 , playableArea, get'
			 , Figure(..), Field(..), Color(..)
			 , Board, Position
			 ) where

data Figure = WS | BS | WQ | BQ | E deriving Show
data Field = Empty | Pawn
data Color = White | Black | None
type Board = [[Figure]]
type Position = (Int, Int)

instance Eq Figure where
    WS == WS = True
    BS == BS = True
    WQ == WQ = True
    BQ == BQ = True
    E == E = True
    _ == _ = False

instance Eq Color where
    White == White = True
    Black == Black = True
    _ == _ = False

getColor :: Figure -> Color
getColor WS = White
getColor WQ = White
getColor BS = Black
getColor BQ = Black
getColor E = None

opposedColor :: Color -> Color
opposedColor color
    | color == White = Black
    | color == Black = White
    | color == None = None

is :: Color -> Position -> Board -> Bool
is White (a,b) board = ((get (a,b) board) == WS) || ((get (a,b) board) == WQ)
is Black (a,b) board = ((get (a,b) board) == BS) || ((get (a,b) board) == BQ)

opposedFigure :: Figure -> Figure
opposedFigure f
    | f == WS = BS
    | f == BS = WS
    | f == BQ = WQ
    | f == WQ = BQ
    | f == E = E

startingBoard :: Board
startingBoard = reverse [[  E, BS,  E, BS,  E, BS,  E, BS],
                         [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ]]

playableArea :: [Position]
playableArea = [(0,0),(2,0),(4,0),(6,0),(1,1),(3,1),(5,1),(7,1),
                (0,2),(2,2),(4,2),(6,2),(1,3),(3,3),(5,3),(7,3),
                (0,4),(2,4),(4,4),(6,4),(1,5),(3,5),(5,5),(7,5),
                (0,6),(2,6),(4,6),(6,6),(1,7),(3,7),(5,7),(7,7)]

{-
    I am using unicode, where:
    26C0 == 9920 is White Pawn icon (in black console we are using it as Black Pawn icon)
    26C1 == 9921 is White Queen icon (in black console we are using it as Black Queen icon)
    26C2 == 9922 is Black Pawn icon (in black console we are using it as White Pawn icon)
    26C3 == 9923 is Black Queen icon (in black console we are using it as White Queen icon)
-}
showFigure :: Figure -> [Char]
showFigure WS = "\9922"
showFigure BS = "\9920"
showFigure WQ = "\9923"
showFigure BQ = "\9921"
showFigure E = "."

{-
    Note that rows are reversed, but it does not affect the game mechanics
-}
showRow :: [Figure] -> [Char]
showRow [] = " \n"
showRow (x : xs) = (showFigure x) ++ " " ++ (showRow xs)

showBoard :: Board -> [Char]
showBoard [] = ""
showBoard (x : xs) = (showRow x) ++ (showBoard xs)

boardInRows :: Board -> [String]
boardInRows board = lines (showBoard board)

showDescription :: [[Char]] -> [[Char]] -> [Char]
showDescription [][] = "  0 1 2 3 4 5 6 7 \n"
showDescription (x : xs) (y : ys) = x ++ y ++ "\n" ++ (showDescription xs ys)

rowsIndex :: [[Char]]
rowsIndex = reverse [ "7 ", "6 ", "5 ", "4 ", "3 ", "2 ", "1 ","0 " ]

sh :: Board -> IO ()
sh board = putStr("  0 1 2 3 4 5 6 7 \n" ++ (showDescription rowsIndex (boardInRows board)))

get :: Position -> [[a]] -> a
get (x,y) board = (board !! y) !! x

get' :: Board -> Position -> Figure
get' board (x,y) = get (x,y) board