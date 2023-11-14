import Data.List
import Text.Printf

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
numLitSeg (hr, mn, dy, mt) = 
    let str = concat [if n < 10 then '0' : show n else show n | n <- [hr, mn, dy, mt]] --converts tuple to a string
    in foldl (\sum char -> sum + charToValue char) 0 str --counts and adds number of lit segments in string and stores them in sum

--converts each character in the string 
charToValue :: Char -> Int
charToValue char = case char of
    '0' -> 6
    '1' -> 2
    '2' -> 5
    '3' -> 5
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 3
    '8' -> 7
    '9' -> 6


--checks if given tuple has no duplicate numbers and the number of lit segments is prime
magic :: (Int, Int, Int, Int) -> Bool
magic (hr, mn, dy, mt) =
    nodups (hr, mn, dy, mt)
    && prime (numLitSeg(hr, mn, dy, mt))

--Checks if given tuple of ints has duplicates in it
nodups :: (Int, Int, Int, Int) -> Bool
nodups (hr, mn, dy, mt) = 
   let nodupsStr = concat [if n < 10 then '0' : show n else show n | n <- [hr, mn, dy, mt]] --convert tuple of ints to string
   in length nodupsStr == length (nub nodupsStr) --compares length of original string with string after removing duplicates if any


--generates list of tuples with hour, minutes, day and month
generator1 :: [(Int, Int, Int, Int)] 
generator1 = 
    [ (hr, mn, dy, mt)
      | hr <- [00 .. 23],
        mn <- [00 .. 59],
        dy <- [01 .. 31],
        mt <- [01 .. 12]
    ]

addMin :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addMin(hr, mn, dy, mt) 
    | mn == 59 = (hr + 1, 00, dy, mt)
    | otherwise = (hr, mn + 1, dy, mt)

tester1 :: (Int, Int, Int, Int) -> Bool
tester1(hr, mn, dy, mt) =
    magic (hr, mn, dy, mt) --tuple is magic
    && magic (hr, mn, dy + 1, mt) --tuple exactly one day later is magic
    && numLitSeg(addMin(hr, mn, dy + 1, mt)) == (numLitSeg(hr, mn, dy, mt)+ numLitSeg(hr, mn, dy + 1, mt)) `div` 2
    -- && numLitSeg(hr, mn, dy, mt) == (numLitSeg(hr, mn + 1, dy + 1, mt) + numLitSeg(hr, mn, dy + 1, mt)) `div` 2 ?


x_tester1 :: Int 
x_tester1 =
    length [t | t <- ts, tester1 t] 
    where
    ts =
        [ ( 6,59,17,24)
        , ( 6,59,17,34)
        , ( 6,59,27,14) , ( 6,59,27,41) , ( 8,59,12,46) , (16,59, 7,24) , (16,59, 7,42)
        , (16,59, 7,43) , (16 ,59 ,27 ,40) , (18,59, 2,46) ]

testmagicday :: (Int, Int, Int, Int) -> Bool
testmagicday(hr, mn, dy, mt) =
    numLitSeg(hr, mn + 1, dy + 1, mt) == (numLitSeg(hr, mn, dy, mt)+ numLitSeg(hr, mn, dy + 1, mt)) `div` 2

main :: IO()
main = 
    print(filter tester1 generator1)
    --print (numLitSeg(17, 0, 28, 4))

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