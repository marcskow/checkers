import Board
import Move
import Utils
import MinMax
import Hitting
import Generate

task a = a + 3
genTree t = t + 2

tree =
    let test = genTree t where t = task a where a = 5 in test + 2


testBoard :: Board
testBoard = reverse     [[  E, BS,  E, BS,  E, BS,  E, BS ],
                         [ BS,  E, BS,  E, BS,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E,  E,  E, WS,  E, WS,  E ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [ WS,  E, WS,  E, WS,  E,  E,  E ],
		                 [  E, WS,  E,  E,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ]]

testBoard' :: Board
testBoard' = reverse    [[  E, BS,  E, BS,  E, BS,  E, BS ],
                         [  E,  E, BS,  E,  E,  E, BS,  E ],
		                 [  E, BS,  E, BS,  E, BS,  E, BS ],
		                 [  E,  E,  E,  E,  E,  E,  E,  E ],
		                 [  E, BS,  E, BS,  E,  E,  E,  E ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ],
		                 [  E, WS,  E, WS,  E, WS,  E, WS ],
		                 [ WS,  E, WS,  E, WS,  E, WS,  E ]]