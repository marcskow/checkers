module Board ( opposedFigure
			 , startingBoard
			 , showFigure
			 , showRow
			 , showBoard
			 , showDescription
			 , boardInRows
			 , rowsIndex
			 , sh
			 , getRow
			 , get
			 , testBoard
			 , Figure(..)
			 , Field(..)
			 ) where

data Figure = WS | BS | WQ | BQ | E deriving Show
data Field = Empty | Pawn

instance Eq Figure where
    WS == WS = True
    BS == BS = True
    WQ == WQ = True
    BQ == BQ = True
    E == E = True
    _ == _ = False

opposedFigure :: Figure -> Figure
opposedFigure f
    | f == WS = BS
    | f == BS = WS
    | f == BQ = WQ
    | f == WQ = BQ
    | f == E = E

{-
    We are using hardcoded board, should it has it's own type ?
    Probably yes
-}
startingBoard :: [[Figure]]
startingBoard = reverse [[  E, BS,  E, BS,  E, BS,  E, BS],
                         [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ]]

testBoard :: [[Figure]]
testBoard = reverse     [[  E, BS,  E, BS,  E, BS,  E, BS ],
                         [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E,  E,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [ WS,  E, WS,  E, WS,  E,  E,  E ],
		                 [  E, WS,  E,  E,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ]]
{-
    We are using unicode, where:
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

showBoard :: [[Figure]] -> [Char]
showBoard [] = ""
showBoard (x : xs) = (showRow x) ++ (showBoard xs)

boardInRows :: [[Figure]] -> [String]
boardInRows board = lines (showBoard board)

showDescription :: [[Char]] -> [[Char]] -> [Char]
showDescription [][] = "  0 1 2 3 4 5 6 7 \n"
showDescription (x : xs) (y : ys) = x ++ y ++ "\n" ++ (showDescription xs ys)

rowsIndex :: [[Char]]
rowsIndex = reverse [ "7 ", "6 ", "5 ", "4 ", "3 ", "2 ", "1 ","0 " ]

sh :: [[Figure]] -> IO ()
sh board= putStr("  0 1 2 3 4 5 6 7 \n" ++ (showDescription rowsIndex (boardInRows board)))

getRow :: Int -> [a] -> a
getRow x board = board !! x

get :: (Int, Int) -> [[a]] -> a
get (x,y) board = (board !! y) !! x
