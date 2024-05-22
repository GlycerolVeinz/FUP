import Data.Char
import Control.Applicative

-- MAZE FUNCTIONS ========================================================================

data Tile = W | F | S deriving (Eq,Show)

data Maze = M [[Tile]]

testingMaze :: Maze   -- a testing Maze
testingMaze = M [[W,W,W,W,W],
          [W,F,W,F,W],
          [W,F,W,W,W],
          [W,F,F,F,W],
          [W,W,W,W,W]]

testingPath :: Path   -- a testing Path
testingPath = [(1,1),(1,2),(1,3),(2,3),(3,3),(4,3),(4,4),(4,5)]

instance Show Maze where
    show (M []) = ""
    show (M (r:rs)) = map displayTile r ++ "\n" ++ show (M rs)
       where displayTile W = '#'
             displayTile F = ' '
             displayTile S = '*'

type Pos = (Int, Int)
type Path = [Pos]
type Task = (Pos,Pos,Maze)

-- Safely puts element at index in list
safePut :: Int -> a -> [a] -> Maybe [a]
safePut n x xs | n < 0 = Nothing
               | n >= length xs = Nothing
               | otherwise = Just $ take n xs ++ [x] ++ drop (n+1) xs

-- Safely gets element at index in list
safeGet :: Int -> [a] -> Maybe a
safeGet n xs | n < 0 = Nothing
             | n >= length xs = Nothing
             | otherwise = Just $ xs !! n

-- Returns the tile at a position in a maze
getTile :: Pos -> Maze -> Maybe Tile
getTile (x,y) (M m) = do
    row <- safeGet y m
    safeGet x row

-- Returns a new maze with replaced tile at a position
setTile :: Pos -> Tile -> Maze -> Maybe Maze
setTile (x,y) t (M m) = do
    row <- safeGet y m
    newrow <- safePut x t row
    M <$> safePut y newrow m

-- Returns maze with path drawn on it
setPath :: Path -> Maze -> Maybe Maze
setPath [] m = Just m
setPath (p:ps) m = do
    m' <- setTile p S m
    setPath ps m'

-- Draws final maze (Maybe maze)
drawSol :: Maze -> Path -> Maze
drawSol m ps = case setPath ps m of
                 Nothing -> m
                 Just m' -> m'

-- BFS FUNCTIONS ========================================================================
neighbs :: Pos -> [Pos]
neighbs (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1),
                 (x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

nextPos :: Pos -> Maze -> [Pos]
nextPos p m = case getTile p m of
                -- if input position is admissible
                -- take all possibilities and filter admissible positions
                Just F -> [ p' | p' <- neighbs p, getTile p' m == Just F]  
                _ -> []

extend :: Path -> Maze -> [Path]
extend [] _ = []
extend path@(p:_) m = map (:path) $ nextPos p m

solve :: Task -> Maybe Path
solve (p,q,m) = bfs [] [[p]] q m

bfs :: [Pos] -> [Path] -> Pos -> Maze -> Maybe Path
bfs _ [] _ _ = Nothing
-- consider the first path in the queue and its head p
bfs visited (path@(p:_):paths) q m
    -- is path a solution? If yes, return the reversed solution
    | p == q = Just $ reverse path
    -- does path end in an already visited position? If yes, disregard it 
    | p `elem` visited = bfs visited paths q m
    -- add p to visited positions and extend path by all possible positions 
    | otherwise = bfs (p:visited) (paths ++ extend path m) q m



