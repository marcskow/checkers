module Generate ( genMoves
        , genNormalMoves, genAllNormalQueenMoves
        , genNormalQueenSteps, genNormalQueenMoves
        , genNormalQueenMove, checkBlockingWay
        , filterNormalMoves, filterOneMove
        , generateHittings
) where

import Board
import Move
import Hitting

genMoves :: Position -> Board -> [Move]
genMoves (a,b) board = let figureType = get (a,b) board
                           hittings = generateHittings figureType (a,b) board
                           in if ((head hittings) == HittingSequence []) then genNormalMoves figureType (a,b) board
                              else generateHittings figureType (a,b) board

genNormalMoves :: Figure -> Position -> Board -> [Move]
genNormalMoves WS (a,b) board = filterNormalMoves (a,b) (genAllNormalMoves WS (a,b)) board
genNormalMoves BS (a,b) board = filterNormalMoves (a,b) (genAllNormalMoves BS (a,b)) board
genNormalMoves WQ (a,b) board = (genAllNormalQueenMoves (a,b) board)
genNormalMoves BQ (a,b) board = (genAllNormalQueenMoves (a,b) board)

genAllNormalMoves :: (Ord a, Num a, Enum a) => Figure -> (a,a) -> [(a,a)]
genAllNormalMoves WS (a,b) = filter (\(x,y) -> Prelude.and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b+f),(a-f,b+f)])
genAllNormalMoves BS (a,b) = filter (\(x,y) -> Prelude.and [x >= 0, x <=7, y >= 0, y <= 7] ) $ [1..1] >>= ( \f -> [(a+f,b-f),(a-f,b-f)])

genAllNormalQueenMoves :: Position -> Board -> [Move]
genAllNormalQueenMoves (a,b) board =
    (genNormalQueenMoves (genNormalQueenSteps LEFT_UP (a,b) board) LEFT_UP (a,b) board) ++
    (genNormalQueenMoves (genNormalQueenSteps LEFT_DOWN (a,b) board) LEFT_DOWN (a,b) board) ++
    (genNormalQueenMoves (genNormalQueenSteps RIGHT_UP (a,b) board) RIGHT_UP (a,b) board) ++
    (genNormalQueenMoves (genNormalQueenSteps RIGHT_DOWN (a,b) board) RIGHT_DOWN (a,b) board)

genNormalQueenSteps :: Direction -> Position -> Board -> [Int]
genNormalQueenSteps dir (a,b) board = [ f | f <- [0..6], (checkBlockingWay f (a,b) dir board) && (isInside f (a,b) dir) && ((get (nextField dir f (a,b)) board) == E)]

genNormalQueenMoves :: [Int] -> Direction -> Position -> t -> [Move]
genNormalQueenMoves [] dir (a,b) board = []
genNormalQueenMoves (step : steps) dir (a,b) board = (genNormalQueenMove step dir (a,b) board) ++ (genNormalQueenMoves steps dir (a,b) board)

genNormalQueenMove :: Int -> Direction -> Position -> t -> [Move]
genNormalQueenMove step dir (a,b) board = [ (Step (a,b)(nextField dir step (a,b)))]

checkBlockingWay :: Int -> Position -> Direction -> Board -> Bool
checkBlockingWay f (x,y) dir board = (length ([ a | a <- [1..f], (isInside a (x,y) dir) && ((get (nextField dir a (x,y)) board) /= E)])) == 0

filterNormalMoves :: Position -> [Position] -> Board -> [Move]
filterNormalMoves (a,b) [] _ = []
filterNormalMoves (a,b) (x : xs) board =
    (filterOneMove (Step (a,b) x)(get x board)) ++ (filterNormalMoves (a,b) xs board)

filterOneMove :: Move -> Figure -> [Move]
filterOneMove (Step (x1,y1)(x2,y2)) t
    | t == E = [(Step (x1,y1)(x2,y2))]
    | otherwise = []

generateHittings :: Figure -> Position -> Board -> [Move]
generateHittings WS (a,b) board = buildPaths(getMaximumHittingPath(buildHittingTree WS True (a,b) board))
generateHittings BS (a,b) board = buildPaths(getMaximumHittingPath(buildHittingTree BS True (a,b) board))
generateHittings WQ (a,b) board = buildPaths(getMaximumHittingPath(buildHittingQueenTree WQ True (a,b) board))
generateHittings BQ (a,b) board = buildPaths(getMaximumHittingPath(buildHittingQueenTree BQ True (a,b) board))
