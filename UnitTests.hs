import Board
import Hitting
import MinMax
import Move
import Utils

tryTests = do
    print $ (move boardBSMove $ Step (1,5)(0,4)) == boardAfterBSStep
    print $ (move boardBSMove $ Hit(7,5)(5,3)) == boardAfterBSHit
    print $ (move boardBSMove $ HittingSequence [Hit (3,5)(5,3), Hit (5,3)(3,1), Hit (3,1)(1,3)]) == boardAfterBSHittingSequence
    print $ (move boardWSMove $ Step (6,2)(7,3)) == boardAfterWSStep
    print $ (move boardWSMove $ Hit (2,2)(0,4)) == boardAfterWSHit
    print $ (move boardWSMove $ HittingSequence [Hit (4,2)(2,4), Hit (2,4)(4,6), Hit (4,6)(6,4)]) == boardAfterWSHittingSequence
    print $ (False == (boardWSMove == boardAfterWSStep))
    print $ (move boardQueen $ Step (5,5)(7,3)) == boardAfterBQStep
    print $ (move boardQueen $ Hit (5,5)(2,2)) == boardAfterBQHit
    print $ (move boardQueen $ Hit (1,1)(4,4)) == boardAfterWQHit

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