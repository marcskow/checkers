module Parser ( parsePosition,
                positions,
                parseToOutput,
                parseMoveToOutput ) where

import Board
import Move

parsePosition :: String -> Position
parsePosition x = positions !! ((read x :: Int)-1)

positions :: [Position]
positions = [(1,7),(3,7),(5,7),(7,7),(0,6),(2,6),(4,6),(6,6),
             (1,5),(3,5),(5,5),(7,5),(0,4),(2,4),(4,4),(6,4),
             (1,3),(3,3),(5,3),(7,3),(0,2),(2,2),(4,2),(6,2),
             (1,1),(3,1),(5,1),(7,1),(0,0),(2,0),(4,0),(6,0)]

parseMoveToOutput :: Move -> String
parseMoveToOutput (Step (x1,y1)(x2,y2)) = ((parseToOutput x1 y1) ++ "-" ++ (parseToOutput x2 y2))
parseMoveToOutput (HittingSequence (x:[])) = let Hit (x1,y1)(x2,y2) = x in ((parseToOutput x1 y1) ++ "x" ++ (parseToOutput x2 y2))
parseMoveToOutput (HittingSequence (x:xs)) = let Hit (x1,y1)(x2,y2) = x in (((parseToOutput x1 y1)) ++ "x" ++ (parseMoveToOutput (HittingSequence xs)))

parseToOutput :: Int -> Int -> String
parseToOutput 1 7 = "1"
parseToOutput 3 7 = "2"
parseToOutput 5 7 = "3"
parseToOutput 7 7 = "4"
parseToOutput 0 6 = "5"
parseToOutput 2 6 = "6"
parseToOutput 4 6 = "7"
parseToOutput 6 6 = "8"
parseToOutput 1 5 = "9"
parseToOutput 3 5 = "10"
parseToOutput 5 5 = "11"
parseToOutput 7 5 = "12"
parseToOutput 0 4 = "13"
parseToOutput 2 4 = "14"
parseToOutput 4 4 = "15"
parseToOutput 6 4 = "16"
parseToOutput 1 3 = "17"
parseToOutput 3 3 = "18"
parseToOutput 5 3 = "19"
parseToOutput 7 3 = "20"
parseToOutput 0 2 = "21"
parseToOutput 2 2 = "22"
parseToOutput 4 2 = "23"
parseToOutput 6 2 = "24"
parseToOutput 1 1 = "25"
parseToOutput 3 1 = "26"
parseToOutput 5 1 = "27"
parseToOutput 7 1 = "28"
parseToOutput 0 0 = "29"
parseToOutput 2 0 = "30"
parseToOutput 4 0 = "31"
parseToOutput 6 0 = "32"