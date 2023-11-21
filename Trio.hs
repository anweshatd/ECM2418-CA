import Data.List

--Checks if given tuple contains any zeros and that each tuple value doesn't have any duplicate numbers
special :: String -> Bool
special n1 =
    notElem '0' n1
    && nodups n1

--Chcecks that each tuple value doesn't have any duplicates  
nodups :: Eq a => [a] -> Bool
nodups x =
    x == nub x

take2 :: [String] -> [String]
take2 n24list =
    let take2 = map (take 2)
        perms = permutations n24list
    in take2 (concatMap take2 perms)

drop1 :: [String] -> [String]
drop1 n24list =
    let drop1 = map (take 2 . reverse)
        perms = permutations n24list
    in drop1 (concatMap drop1 perms)

generator2 :: [(String, String, String, String, String)]
generator2 =
    [ (n1, n2, n3, n4, n5)
      | n1 <- map show [123 .. 987],
        special n1,
        n2 <- take2 n1,
        n3 <- permutations n1,
        n4 <- drop1 n1,
        n5 <- permutations n1
        --nodups [n1, n2, n3, n4, n5]
    ]

x_generator2 :: Int
x_generator2 =
    length [t | t <- ts, t `elem` g]
    where
    g = generator2
    ts =
        [ ("123","21","123","12","123")
        , ("162","26","261","12","621") , ("219","19","912","21","291") , ("329","92","932","32","239") , ("439","94","394","43","394") , ("549","95","945","95","945")
        , ("568","68","586","56","586")
        , ("769","67","679","97","796")
        , ("879","79","897","98","789")
        , ("987","79","789","79","789") ]

tester2 :: (String, String, String, String, String) -> Bool
tester2(n1, n2, n3, n4, n5) =
    let [nn1, nn2, nn3, nn4, nn5] = map read [n1, n2, n3, n4, n5]
    in nn1 - nn2 == nn3
    && nn3 - nn4 == nn5
    && nn1 + nn3 + nn5 < 2000


main :: IO ()
main = do
    --print(filter tester2 generator2)
    --print generator2
    let n1 = map show [123 .. 987]
    let x = ["987", "999", "123"]
    --print (map (take 2) x)
    --print (map (drop (length (head x) - 2)) n1)
    print (drop1 n1)

-- make a new funtion to make a list of permutations pf n1 and the make another list taking the first two digits of each nukber in that new list and save in n2 and n4 and check of the first digit of n2 is not the same as the first dogit of n2