import Board
import Move
import Utils
import MinMax
import Hitting
import Generate
import Control.Applicative
import Control.Monad

data Problem a = Ok a | Error String

-- instance Monad Problem where
--    (>>=)

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

wielomian :: [Double] -> Double -> Double
wielomian [] x = 0
wielomian (a:as) x =
    let len = length (a:as)
    in (a * (x ^ (len - 1))) + (wielomian as x)
-- wielomian [wyrazWolny] argument = wyrazWolny

main = fmap reverse getLine >>= putStrLn

main' = getLine >>= \a ->
       getLine >>= \b ->
       getLine >>= \c ->
       putStrLn (reverse c ++ "\n" ++ reverse b ++ "\n" ++ reverse a)

main'' = do
    sentence <- getLine
    (print . length) $ words sentence

main''' = length <$> (words <$> getLine)

main'''' = liftA2 (++) getLine getLine

main2 = getLine >>= \x -> print $ length . words $ x

noWhiteSpace = getLine >>= \x -> return . concat . words $ x

noWhiteSpace' = do
    row <- getLine
    print . concat $ words row

main3 = do
    row <- getLine
    return . length . words $ row


foo1 f v [] = v
foo1 f v (x:xs) = foo1 f (f v x) xs

foo3 = filter (==0)
foo4 v1 v2 = v1 * v2

foo5 [] = ([],[])
foo5 ((x1,x2):xs) = (x1:ys,x2:zs) where (ys,zs) = foo5 xs

half x = if even x
          then Just (x `div` 2)
          else Nothing

function a x = liftM2 (+) a x

mySum = do
    number <- getLine
    print(sum $ map (\x -> read[x]) number)

nextTextFunction = do
    putStrLn "Czesc"
    x <- getLine
    putStrLn x
    putChar '\n'

nextTextFunction' = putStrLn "Czesc" >> (getLine >>= putStrLn) >> putChar '\n'

-- data CalculationContext a = Result a | Errore {mess::String,info::String}

-- instance Monad CalculationContext where
--    return = Result
--    Result a >>= f = f a
--    Errore id msg >>= _ = Errore id msg

