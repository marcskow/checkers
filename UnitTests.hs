{- Autor: Marcin Skowron, Informatyka II rok -}
import Board
import Hitting
import Generate
import MinMax
import Move
import Utils

{-
    Unit tests. Features tested:
        - move :: Board -> Move -> Board -- Defined at Move.hs
        - updateBoard :: Board -> Figure -> Position -> Board -- Defined at Move.hs
        - genMoves :: Position -> Board -> [Move]
        - minmax :: Color -> Color -> Int -> Board -> Move
    Function print warning only if test fails.
-}
tryTests :: IO ()
tryTests = do
    printTest "1" $ (move boardBSMove $ Step (1,5)(0,4)) == boardAfterBSStep
    printTest "2" $ (move boardBSMove $ Hit(7,5)(5,3)) == boardAfterBSHit
    printTest "3" $ (move boardBSMove $ HittingSequence [Hit (3,5)(5,3), Hit (5,3)(3,1), Hit (3,1)(1,3)]) == boardAfterBSHittingSequence
    printTest "4" $ (move boardWSMove $ Step (6,2)(7,3)) == boardAfterWSStep
    printTest "5" $ (move boardWSMove $ Hit (2,2)(0,4)) == boardAfterWSHit
    printTest "6" $ (move boardWSMove $ HittingSequence [Hit (4,2)(2,4), Hit (2,4)(4,6), Hit (4,6)(6,4)]) == boardAfterWSHittingSequence
    printTest "7" $ (False == (boardWSMove == boardAfterWSStep))
    printTest "8" $ (move boardQueen $ Step (5,5)(7,3)) == boardAfterBQStep
    printTest "9" $ (move boardQueen $ Hit (5,5)(2,2)) == boardAfterBQHit
    printTest "10" $ (move boardQueen $ Hit (1,1)(4,4)) == boardAfterWQHit
    printTest "11" $ (move boardBeforeBSHittingsToQueen $ HittingSequence [Hit (4,4)(2,2), Hit (2,2)(0,0)]) == boardAfterBSHittingsToQueen
    printTest "12" $ (move emptyBoard $ Step (0,0)(1,1)) == emptyBoard
    printTest "13" $ (get' (updateBoard emptyBoard WS (0,0)) (0,0)) == WS
    printTest "14" $ (get' (move boardBeforeSToQStep (Step (0,6)(1,7))) (1,7)) == WQ
    printTest "15" $ (get' (move boardBeforeSToQStep (Step (0,1)(1,0))) (1,0)) == BQ
    printTest "16" $ (get' (move boardBeforeSToQStep (HittingSequence[Hit(7,2)(5,0),Hit(5,0)(3,2)])) (3,2)) == BS
    printTest "17" $ (get' (move boardBeforeSToQStep (HittingSequence[Hit(3,5)(5,7),Hit(5,7)(7,3)])) (7,3)) == WS
    printTest "18" $ (get' (move boardBeforeSToQHit (HittingSequence[Hit(5,4)(3,2),Hit(3,2)(1,0)])) (1,0)) == BQ
    printTest "19" $ (get' (move boardBeforeSToQHit (HittingSequence[Hit(0,3)(2,5),Hit(2,5)(4,7)])) (4,7)) == WQ
    printTest "20" $ (genMoves (0,2) boardWSMove) == [HittingSequence [Hit(0,2)(2,4),Hit(2,4)(4,6),Hit(4,6)(6,4)]]
    printTest "21" $ (HittingSequence [Hit(0,2)(2,4),Hit(2,4)(0,6)]) `notElem` (genMoves (0,2) boardWSMove)
    printTest "22" $ (minmax White White 3 minmaxBoard) == (HittingSequence [Hit(0,2)(2,4),Hit(2,4)(4,6),Hit(4,6)(6,4)])

printTest :: String -> Bool -> IO ()
printTest nr test = if(test == True) then putStr "" else putStrLn ("Test failed: " ++ nr)

boardWSMove :: Board
boardWSMove =           [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, BS,  E, BS,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E, BS,  E,  E,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardAfterWSStep :: Board
boardAfterWSStep =      [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E,  E,  E, WS ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E, BS,  E,  E,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardAfterWSHit :: Board
boardAfterWSHit =         [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E,  E,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E, BS,  E,  E,  E,  E ],
		                 [ WS,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E, BS,  E,  E,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]


boardAfterWSHittingSequence :: Board
boardAfterWSHittingSequence =   [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E,  E,  E, WS,  E ],
		                 [  E, BS,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E, WS,  E ],
		                 [  E, BS,  E,  E,  E,  E,  E, BS ],
		                 [  E,  E, BS,  E,  E,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardBSMove :: Board
boardBSMove =           [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E,  E,  E, WS,  E,  E ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E, WS,  E, WS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]


boardAfterBSStep :: Board
boardAfterBSStep =      [[ WS,  E, WS,  E, WS,  E, WS,  E ],
                         [  E, WS,  E,  E,  E, WS,  E,  E ],
                         [ WS,  E, WS,  E, WS,  E, WS,  E ],
                         [  E,  E,  E,  E,  E,  E,  E,  E ],
                         [ BS,  E,  E,  E, WS,  E, WS,  E ],
                         [  E,  E,  E, BS,  E, BS,  E, BS ],
                         [ BS,  E, BS,  E, BS,  E, BS,  E ],
                         [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardAfterBSHit :: Board
boardAfterBSHit =       [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E,  E,  E, WS,  E,  E ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E,  E,  E, BS,  E,  E ],
		                 [  E,  E,  E,  E, WS,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E,  E ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]


boardAfterBSHittingSequence :: Board
boardAfterBSHittingSequence =
                        [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E,  E,  E, WS,  E,  E ],
		                 [ WS,  E,  E,  E,  E,  E, WS,  E ],
		                 [  E, BS,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E, WS,  E ],
		                 [  E, BS,  E,  E,  E, BS,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardBeforeBSHittingsToQueen :: Board
boardBeforeBSHittingsToQueen =
                        [[  E,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E,  E,  E, WS,  E,  E ],
		                 [ WS,  E,  E,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E, WS,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E, BS,  E, WS,  E ],
		                 [  E,  E,  E, BS,  E,  E,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardAfterBSHittingsToQueen :: Board
boardAfterBSHittingsToQueen =
                        [[ BQ,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E,  E,  E, WS,  E,  E ],
		                 [ WS,  E,  E,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E, WS,  E ],
		                 [  E,  E,  E, BS,  E,  E,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardQueen :: Board
boardQueen =            [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WQ,  E, WS,  E, WS,  E, WS ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E, WS,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BQ,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]


boardAfterWQHit :: Board
boardAfterWQHit =     [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E, WS,  E, WS,  E, WS ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  WQ,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BQ,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

boardAfterBQHit :: Board
boardAfterBQHit =       [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WQ,  E, WS,  E, WS,  E, WS ],
		                 [  E,  E, BQ,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E,  E,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]


boardAfterBQStep :: Board
boardAfterBQStep =      [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WQ,  E, WS,  E, WS,  E, WS ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E,  E,  E, WS,  E,  E,  E, BQ ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E,  E,  E, BS ],
		                 [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

minmaxBoard :: Board
minmaxBoard =           [[ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E,  E,  E, WS,  E ],
		                 [  E, BS,  E, BS,  E,  E,  E,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E, BS,  E,  E,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ]]

emptyBoard = [[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E]]
boardBeforeSToQHit = [[E,E,E,E,E,E,E,E],[E,E,WS,E,E,E,E,E],[E,E,E,E,E,E,E,E],[WS,E,E,E,WS,E,E,E],[E,BS,E,E,E,BS,E,E],[E,E,E,E,E,E,E,E],[E,E,E,BS,E,E,E,E],[E,E,E,E,E,E,E,E]]
boardBeforeSToQStep = [[E,E,E,E,E,E,E,E],[BS,E,E,E,WS,E,WS,E],[E,E,E,E,E,E,E,BS],[E,E,E,E,E,E,E,E],[E,E,E,E,E,E,E,E],[E,E,E,WS,E,E,E,E],[WS,E,E,E,BS,E,BS,E],[E,E,E,E,E,E,E,E]]
