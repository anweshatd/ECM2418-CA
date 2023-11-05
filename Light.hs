import Data.List

-- Function to check if given number is prime
prime :: Int -> Bool
prime =
    not . factorisable 2

factorisable :: Int -> Int -> Bool
factorisable f n
    | f * f <= n = n `mod` f == 0 || factorisable (f + 1) n
    | otherwise = False


--calculates number of lit segements in given tuple
numLitSeg :: (Int, Int, Int, Int) -> Int
numLitSeg (hr, mn, dy, mt) 


--checks if given tuple has no duplicate numbers and the number of lit segments is prime
magic :: (Int, Int, Int, Int) -> Bool
magic (hr, mn, dy, mt) =
    nodups (hr, mn, dy, mt)
    && prime (numLitSeg(hr, mn, dy, mt))

--Checks if given tuple of ints has duplicates in it
nodups :: Eq a => (a, a, a, a) -> Bool
nodups (hr, mn, dy, mt) =
    (hr, mn, dy, mt) == nub (hr, mn, dy, mt)


--generates list of tuples with hour, minutes, day and month
generator1 :: [(Int, Int, Int, Int)] 
generator1 = 
    [ (hr, mn, dy, mt)
      | hr <- [00 .. 23],
        mn <- [00 .. 59],
        dy <- [01 .. 365],
        mt <- [01 .. 12]
    ]


tester1 :: (Int, Int, Int, Int) -> Bool
tester1(hr, mn, dy, mt) =
    magic (hr, mn, dy, mt)
    && magic (hr + 24, mn, dy + 1, mt)
    && numLitSeg(hr + 24, mn + 1, dy + 1, mt) == (numLitSeg(hr, mn, dy, mt)+ numLitSeg(hr + 24, mn, dy + 1, mt)) `div` 2


main :: IO()
main = 
    print(filter tester1 generator1)

{-
--testing

numLitSeg :: (Int, Int, Int, Int) -> String
numLitSeg(hr, mn, dy, mt) =
    concat [show hr, show mn, show dy, show mt]
    
main :: IO ()
main = 
    print (numLitSeg(09, 43, 28, 06))


    tupleToString :: (Int, Int, Int, Int) -> String
tupleToString(hr, mn, dy, mt) =
    concat [show hr, show mn, show dy, show mt]

numLitSeg :: Char -> Int -> Int
numLitSeg char sum 
    | char `elem` ['0'..'9'] && char `elem` "235" = sum + 5
    | char `elem` ['0'..'9'] = sum + case char of
        '0' -> 6
        '1' -> 2
        '4' -> 4
        '6' -> 6
        '7' -> 3
        '8' -> 7
        '9' -> 6
    | otherwise = sum
    

main :: IO ()
main = do
    let str = tupleToString(09, 43, 28, 06)
    let sum = foldr numLitSeg 0 str
    print (str)

import Text.Printf

tupleToString :: (Int, Int, Int, Int) -> String
tupleToString (hr, mn, dy, mt) = printf "%02d%02d%02d%02d" hr mn dy mt

 -}
