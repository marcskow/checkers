import Board
import Hitting
import MinMax
import Move
import Utils

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

printTest nr test = if(test == True) then putStr "" else putStrLn ("Test failed: " ++ nr)

emptyBoard :: Board
emptyBoard =   [[E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E],
                [E,E,E,E,E,E,E,E]]

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