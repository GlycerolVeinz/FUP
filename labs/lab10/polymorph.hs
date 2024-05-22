data DualNum a = DN a a deriving (Eq, Ord)

instance Show a => Show (DualNum a) where
    show (DN x x') = show x ++ " + " ++ show x' ++ "eps"

instance Num a => Num (DualNum a) where
    (DN x x') + (DN y y') = DN (x + y) (x' + y')
    (DN x x') - (DN y y') = DN (x - y) (x' - y')
    (DN x x') * (DN y y') = DN (x * y) (x * y' + y * x')
    negate (DN x x') = DN (negate x) (negate x')
    abs (DN x x') = DN (abs x) (x' * signum x)
    signum (DN x x') = DN (signum x) 0
    fromInteger x = DN (fromInteger x) 0

instance Fractional a => Fractional (DualNum a) where
    (DN x x') / (DN y y') = DN (x/y) ((x'*y - x*y') / (y*y))
    fromRational r = DN (fromRational r) 0

sqr :: (Fractional a, Ord a) => a -> a
sqr x = convAbs $ iterate improve 1
  where improve r = (r + x/r) / 2
        convAbs (x1:x2:xs) | abs (x1-x2) < 1e-10 = x2
                           | otherwise = convAbs xs


-- look at :i Floating to know what are the minimium functions to implement
instance (Floating a) => Floating (DualNum a) where
    pi               = DN pi 0
    exp (DN x x')    = DN r (r*x') where r = exp x 
    log (DN x x')    = DN (log x) (x' / x)
    sqrt (DN x x')   = DN r (x' / (2 * r)) where r = sqrt x
    sin (DN x x')    = DN (sin x) (x' * cos x) 
    cos (DN x x')    = DN (cos x) (-x' * sin x) 
    acos (DN x x')   = DN (acos x) (-x' / sqrt(1 - x*x))
    asin (DN x x')   = DN (asin x) ( x' / sqrt(1 - x*x))
    atan (DN x x')   = DN (atan x) ( x' / (1 + x*x))
    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2


interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

permutations :: [a] -> [[a]]
permutations [] = [[]]
-- concatMap == concat (map func list)
permutations (x:xs) = concatMap (interleave x) (permutations xs)


type Edge a = (a,a) 
data Graph a = Graph {vertices :: [a], edges :: [Edge a]} deriving Show

gr :: Graph Int
gr = Graph {vertices=[1..6], edges=[(1, 2), (1, 5), (2, 3), (2, 5), (3, 4), (4, 5), (4, 6)]}

isEdge :: Eq a => (a, a) -> Graph a -> Bool 
isEdge (a, b) gr = (a, b) `elem` edgs || (b, a) `elem` edgs
                        where edgs = (edges gr)

-- isPath :: Eq a => [a] -> Bool


