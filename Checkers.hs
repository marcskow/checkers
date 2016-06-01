import qualified Data.Foldable as F
import Data.Monoid
import Data.List
import Data.Function
import Data.Ord
import Data.Tree
import Board
import Move
import Hitting
import PlayerGame

data Color = White | Black


instance Eq Color where
    White == White = True
    Black == Black = False
    _ == _ = False

opposedColor color
    | color == White = Black
    | color == Black = White

buildSubForest color depth [] = []
buildSubForest color depth (map : maps) = [Node map [generatePlayTree (opposedColor color) (depth - 1) map]] ++ buildSubForest color depth maps

listtree' (Node label []) = [[label]]
listtree' (Node label xs) = map (label:) $ concat $ map listtree' xs

generatePlayTree color 0 actualmap = Node actualmap []
generatePlayTree color depth actualmap =
    let moves = genAllPossibleMoves playableArea color actualmap
        maps = genAllPossibleMaps moves color actualmap
        subforest = buildSubForest color depth maps
        in Node actualmap subforest

{-
buildHittingTree :: Figure -> Bool -> Position -> Board -> HittingTree Position
buildHittingTree w False (x,y) board = Nil
buildHittingTree w t (x,y) board =
    let next dir = (buildHittingTree w True (nextField dir 2 (x,y)) (move board (Hit (x,y) (nextField dir 2 (x,y)))))
        leftTreeUp =    if (canHit w LEFT_UP (x,y) board) then next LEFT_UP else Nil
        leftTreeDown =  if (canHit w LEFT_DOWN (x,y) board) then next LEFT_DOWN else Nil
        rightTreeUp =   if (canHit w RIGHT_UP (x,y) board) then next RIGHT_UP else Nil
        rightTreeDown = if (canHit w RIGHT_DOWN (x,y) board) then next RIGHT_DOWN else Nil
    in HittingNode (x,y) [ leftTreeUp, leftTreeDown, rightTreeUp, rightTreeDown ]
-}

playableArea :: [Position]
playableArea = [(0,0),(2,0),(4,0),(6,0),(1,1),(3,1),(5,1),(7,1),
                (0,2),(2,2),(4,2),(6,2),(1,3),(3,3),(5,3),(7,3),
                (0,4),(2,4),(4,4),(6,4),(1,5),(3,5),(5,5),(7,5),
                (0,6),(2,6),(4,6),(6,6),(1,7),(3,7),(5,7),(7,7)]

is White (a,b) board = ((get (a,b) board) == WS) || ((get (a,b) board) == WQ)
is Black (a,b) board = ((get (a,b) board) == BS) || ((get (a,b) board) == BQ)

genAllPossibleMoves [] c actual = []
genAllPossibleMoves (f : fields) c actual = let thisFieldMoves = if (is c f actual) then (genMoves f actual) else []
                                            in thisFieldMoves ++ (genAllPossibleMoves fields c actual)

genAllPossibleMaps [] c actual = []
genAllPossibleMaps (m : moves) c actual = [(move actual m)] ++ (genAllPossibleMaps moves c actual)

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
