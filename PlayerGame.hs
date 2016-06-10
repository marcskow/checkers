module PlayerGame ( convert) where

import Data.Char
import Board
import Move
import MinMax
import Data.List.Split

playerVsPlayer board color = do
    if(isOver color board) then do
         sh $ reverse board
         putStrLn "Game over"
    else do
         sh $ reverse board
         if(color == White) then putStrLn "White move, instructions: (step) e.g. 10-11 (hit) e.g. 10x11x12"
         else putStrLn "Black move, instructions: (step) e.g. 10-11 (hit) e.g. 10x11x12"
         line <- getLine
         let inputMove = (myParser line)
         if(inputMove `notElem` (genAllPossibleMoves playableArea White board)) then do
            putStrLn "You can't move like this, maybe you should hit or it's not valid move"
            playerMove board color
         else do
         let board' = move board inputMove
         playerVsPlayer board' (opposedColor color)

playerVsComputer board playerColor = do
    if(isOver playerColor board) then do
         sh $ reverse board
         putStrLn $ "You lose, and computer win"
    else do
         sh $ reverse board
         putStrLn "Your move: (step) 10-11 (hit) 10x11x12"
         line <- getLine
         let inputMove = (myParser line)
         if(inputMove `notElem` (genAllPossibleMoves playableArea White board)) then do
            putStrLn "You can't move like this, maybe you should hit or it's not valid move"
            playerMove board playerColor
         else do
         let board' = move board inputMove
         computerMove board' playerColor

computerMove board playerColor = do
        if(isOver (opposedColor playerColor) board) then do
                                    sh $ reverse board
                                    putStrLn "You win, computer lose"
                                                  else playerMove (move board (minmax (opposedColor playerColor) (opposedColor playerColor) 4 board)) playerColor


isOver color board = ((minmax color color 0 board) == (Step (0,0)(0,0)))

version =  [[E,E,E,E,E,E,E,E],
            [E,E,E,E,E,E,E,WS],
            [E,E,E,E,E,E,E,E],
            [E,E,E,BQ,E,E,E,E],
            [E,E,WS,E,E,E,E,E],
            [E,E,E,E,E,E,E,E],
            [WQ,E,E,E,E,E,E,E],
            [E,E,E,WQ,E,E,E,E]]

myParser string
    | (('-' `elem` string) && (isValid string)) = let words = (splitOn "-" string)
                                                      from = words !! 0
                                                      to = words !! 1
                                                  in Step (parsePosition from)(parsePosition to)
    | (('x' `elem` string) && (isValid string)) = let words = (splitOn "x" string)
                                                      sequence = (hits (words))
                                                  in HittingSequence sequence
    | otherwise = (Step (0,-1)(0,-1))

isValid string
    | ('-' `elem` string) = areWordsValid(splitOn "-" string)
    | ('x' `elem` string) = areWordsValid(splitOn "x" string)
    | otherwise = False

areWordsValid [] = True
areWordsValid (w:ws) = (isWordValid w) && (areWordsValid ws)
isWordValid w = w `elem` movesToValidation

movesToValidation = ["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
         "16","17","18","19","20","21","22","23","24","25","26","27","28","29","30",
         "31","32"]
{-
myParser string = if(('-' `elem` string)) then let words = (splitOn "-" string)
                                                   from = words !! 0
                                                   to = words !! 1
                                                   in Step (parsePosition from)(parsePosition to)
                else let words = (splitOn "x" string)
                         sequence = (hits (words))
                         in HittingSequence sequence
-}

minmaxBoard2 :: Board
minmaxBoard2 = [[WS,E,E,E,BQ,E,WS,E],
                [E,E,E,E,E,E,E,WS],
                [BS,E,WS,E,E,E,E,E],
                [E,WS,E,E,E,E,E,BS],
                [BS,E,WS,E,E,E,E,E],
                [E,E,E,BS,E,E,E,BS],
                [BS,E,BS,E,BS,E,E,E],
                [E,E,E,BS,E,E,E,BS]]

hits [] = []
hits (x:[]) = []
hits (x1:x2:xs) = (Hit(parsePosition x1)(parsePosition x2)) : (hits (x2:xs))

parsePosition x = positions !! ((read x :: Int)-1)

positions = [(1,7),(3,7),(5,7),(7,7),(0,6),(2,6),(4,6),(6,6),
             (1,5),(3,5),(5,5),(7,5),(0,4),(2,4),(4,4),(6,4),
             (1,3),(3,3),(5,3),(7,3),(0,2),(2,2),(4,2),(6,2),
             (1,1),(3,1),(5,1),(7,1),(0,0),(2,0),(4,0),(6,0)]

convert [] = []
convert (x : xs) = x : convert xs
