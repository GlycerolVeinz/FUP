separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = (x:fst (separate xs), y:snd (separate xs))


{-
first int is a nmber to be changed
second int is a base to be changed to
string is the result of the conversion
-}
changeIntToOtherBaseStr :: Int -> Int -> String
changeIntToOtherBaseStr n radix = if n < radix then [chars !! n] 
                                    else changeIntToOtherBaseStr (div n radix) radix ++ [chars !! (mod n radix)]
                                    where chars = ['0'..'9'] ++ ['A'..'Z']


split :: Int -> [Int] -> [[Int]]
split n [] = []
split n xs = if length xs < n then [xs] 
                else take n xs : split n (drop n xs)

{-
Average each n elements in a list
-}
average_n :: Int -> [Int] -> [Float]
average_n n xs = map (\x -> fromIntegral (sum x) / fromIntegral n) (split n xs)

copy :: Int -> String -> String
copy 0 _ = ""
copy n str = str ++ copy (n - 1) str

luhnDouble :: Int -> Int
luhnDouble n = if (n * 2) > 9 then (n * 2) - 9 else (n * 2)

luhn :: [Int] -> Bool
luhn cardNum = if ((sum (map luhnDouble everyOther)) + (sum everyFirst)) `mod` 10 == 0 then True else False
                where (everyFirst, everyOther) = separate (reverse cardNum) 

