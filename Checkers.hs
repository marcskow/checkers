{- Autor: Marcin Skowron, Informatyka II rok -}
module Checkers () where

import Data.Char
import Board
import Move
import MinMax
import Parser
import Data.List.Split
import Control.Concurrent

{-
    This function allows us to play checkers in player vs player mode.
    First move is for the player whose color is given in parameter.
    If player makes invalid move, there will an error appear, and player should repeat his
    move by choosing a valid one. Game ends when one of players cannot make any move
    (So in the case of a tie or when one player has lost all his pawns)
-}
playerVsPlayer :: Board -> Color -> IO ()
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
         if(inputMove `notElem` (genAllPossibleMoves playableArea color board)) then do
            putStrLn "You can't move like this, maybe you should hit or it's not valid move"
            playerVsPlayer board color
         else do
         let board' = move board inputMove
         playerVsPlayer board' (opposedColor color)

{-
    This function allows us to play checkers in player vs computer mode.
    First move is for the player whose color is given in parameter.
    If player makes invalid move, there will an error appear, and player should repeat his
    move by choosing a valid one. Game ends when player or computer cannot make any move
    (So in the case of a tie or when player or computer has lost all his pawns)
-}
playerVsComputer :: Board -> Color -> Int -> IO ()
playerVsComputer board playerColor mmDepth = do
    if(playerColor == White) then playerMove board playerColor mmDepth
    else computerMove board playerColor mmDepth

playerMove :: Board -> Color -> Int -> IO ()
playerMove board playerColor mmDepth = do
    if(isOver playerColor board) then do
         sh $ reverse board
         putStrLn $ "You lose, and computer win"
    else do
         sh $ reverse board
         putStrLn "Your move: (step) 10-11 (hit) 10x11x12"
         line <- getLine
         let inputMove = (myParser line)
         if(inputMove `notElem` (genAllPossibleMoves playableArea playerColor board)) then do
            putStrLn "You can't move like this, maybe you should hit or it's not valid move"
            playerMove board playerColor mmDepth
         else do
         let board' = move board inputMove
         sh $ reverse board'
         computerMove board' playerColor mmDepth

computerMove :: Board -> Color -> Int -> IO ()
computerMove board playerColor mmDepth = do
        if(isOver (opposedColor playerColor) board) then do
            sh $ reverse board
            putStrLn "You win, computer lose"
        else do
        let minMaxMove = (minmax (opposedColor playerColor) (opposedColor playerColor) 4 board)
        putStrLn $ "Computer move: " ++ (parseMoveToOutput minMaxMove)
        playerMove (move board minMaxMove) playerColor mmDepth

{-
    This function allows us to show checkers game simulation as computer vs computer mode.
    Game ends when White or Black cannot make any move. You are giving MinMax depth as parametr.
    You are also giving threadDelay (microseconds) as parameter so that you can observe computer moves if MinMax works too fast.
    (So in the case of a tie or when player or computer has lost all his pawns)
-}
computerVsComputer :: Board -> Color -> Int -> Bool -> IO ()
computerVsComputer board color mmDepth shBoard = do
        if(isOver color board) then do
            if(shBoard == True) then sh $ reverse board
            else do
            if(color == White) then putStrLn "Black computer win"
            else putStrLn "White computer win"
        else do
            let minMaxMove = (minmax color color mmDepth board)
                board' = (move board minMaxMove)
            if(shBoard == True) then do
                sh $ reverse board
                putStrLn $ parseMoveToOutput minMaxMove
                computerVsComputer board' (opposedColor color) mmDepth shBoard
            else do
                putStrLn $ parseMoveToOutput minMaxMove
                computerVsComputer board' (opposedColor color) mmDepth shBoard

isOver :: Color -> Board -> Bool
isOver color board = ((minmax color color 0 board) == (Step (0,0)(0,0)))

myParser :: String -> Move
myParser string
    | (('-' `elem` string) && (isValid string)) =
        let words = (splitOn "-" string)
            from = words !! 0
            to = words !! 1
        in Step (parsePosition from)(parsePosition to)
    | (('x' `elem` string) && (isValid string)) =
        let words = (splitOn "x" string)
            sequence = (hits (words))
        in HittingSequence sequence
    | otherwise = (Step (0,-1)(0,-1))

isValid :: String -> Bool
isValid string
    | ('-' `elem` string) = areWordsValid(splitOn "-" string)
    | ('x' `elem` string) = areWordsValid(splitOn "x" string)
    | otherwise = False

areWordsValid :: [String] -> Bool
areWordsValid [] = True
areWordsValid (w:ws) = (w `elem` movesToValidation) && (areWordsValid ws)

movesToValidation :: [String]
movesToValidation = ["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15",
         "16","17","18","19","20","21","22","23","24","25","26","27","28","29","30",
         "31","32"]

hits :: [String] -> [Move]
hits [] = []
hits (x:[]) = []
hits (x1:x2:xs) = (Hit(parsePosition x1)(parsePosition x2)) : (hits (x2:xs))
