import Data.List

-- Function to check if given number is prime
prime :: Int -> Bool
prime =
    not . factorisable 2

factorisable :: Int -> Int -> Bool
factorisable f n
    | f * f <= n = n `mod` f == 0 || factorisable (f + 1) n
    | otherwise = False


--calculates and returns number of lit segements in given tuple
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

--adds minute to tuple 
addMin :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addMin(hr, mn, dy, mt) 
    | mn == 59 = (hr + 1, 00, dy, mt) --increments hour by 1 and makes minutes 00 if given tuple's minutes are 59
    | otherwise = (hr, mn + 1, dy, mt) --increments minutes by 1 if minutes < 59

tester1 :: (Int, Int, Int, Int) -> Bool
tester1(hr, mn, dy, mt) =
    magic (hr, mn, dy, mt) --tuple is magic
    && magic (hr, mn, dy + 1, mt) --tuple exactly one day later is magic
    && numLitSeg(addMin(hr, mn, dy + 1, mt)) == (numLitSeg(hr, mn, dy, mt)+ numLitSeg(hr, mn, dy + 1, mt)) `div` 2


main :: IO()
main = 
    print(filter tester1 generator1)
