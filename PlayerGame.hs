module PlayerGame ( gameLoop
                  , drawScene
                  , isFinalState
                  , convert) where

import Data.Char
import Board
import Move

gameLoop board | isFinalState board = sh board
               | otherwise = do
                    drawScene board
                    str <- readLn :: IO [Int]
                    gameLoop (move board (Step (str !! 0, str !! 1 )(str !! 2, str !! 3)))

isFinalState board = False
drawScene board = sh board

convert [] = []
convert (x : xs) = x : convert xs
