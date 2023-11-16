import Data.List

--Checks if given tuple contains any zeros and that each tuple value doesn't have any duplicate numbers
special :: (Int, Int, Int, Int, Int) -> Bool
special(n1, n2, n3, n4, n5) =
    notElem 0 [n1, n2, n3, n4, n5]
    && nodups(n1, n2, n3, n4, n5)

--Chcecks that each tuple value doesn't have any duplicates  
nodups :: (Int, Int, Int ,Int, Int) -> Bool
nodups(n1, n2, n3, n4, n5) =
    all (\x -> length x == length (nub x)) [[n1], [n2], [n3], [n4], [n5]]


generator2 :: [(Int, Int, Int, Int, Int)]
generator2 =
    [ (n1, n2, n3, n4, n5)
      | n1 <- [102 .. 987],
        n2 <- [12 .. 98], --(take 2) permutatons of n1
        n3 <- permutations n1,
        n4 <- [12 .. 98],
        n5 <- permutations n1,
        special(n1, n2, n3, n4, n5)
    ]

tester2 :: (Int, Int, Int, Int, Int) -> Bool
tester2(n1, n2, n3, n4, n5) =
    n1 - n2 == n3
    && n3 - n4 == n5
    && n1 + n3 + n5 < 2000

main :: IO ()
main = 
    print(filter tester2 generator2)
    --print(special(123, 21, 123, 12, 123))