-- This is a new dataType definition
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Overloading function "show"
instance (Show a) => Show (Tree a) where
  show (Leaf a) = "<Leaf " ++ show a ++ "/>"
  show (Node l r) = "<Node> " ++ show l ++ show r ++ " </Node>"


treeExample :: Tree Char
treeExample = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')


treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node l r ) = (max (treeDepth l) (treeDepth r)) + 1  
            
labelTreeHelper :: Tree a -> Int -> (Tree (a, Int), Int)
labelTreeHelper (Leaf val) acc = (Leaf (val, acc), acc + 1)
labelTreeHelper (Node l r) acc = (Node leftSolved rightSolved, accFromRight)
  where (leftSolved, accFromLeft) = labelTreeHelper l acc
        (rightSolved, accFromRight) = labelTreeHelper r accFromLeft


labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (labelTreeHelper t 0)


type Monomial a = (a, Int)
-- data Polynomial a = Null | Monomial a | Pol (Polynomial a) (Polynomial a)
data Polynomial a = Null | Pol (Monomial a) (Polynomial a)

format :: (Show a, Ord a, Num a) => Monomial a -> String
format (c, e) | e == 0 = display c
              | otherwise = display c ++ "x^" ++ show e
              where display k | k < 0 >= show k
                              | otherwise = "(" ++ show k ++ ")"

instance (Show a, Ord a, Num a) => Show (Polynomial a) where
  show Null = "0"
  show (Pol m Null) = format m
  show (Pol m p) = format m ++ " + " ++ show p

getDegree :: Polynomial a -> Int
getDegree pol = iter pol (-1) where
    iter Null n = n
    iter (Pol (_, e) ms) n | e > n = iter ms e
                           | otherwise = iter ms n



