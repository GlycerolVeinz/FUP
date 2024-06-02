import FTree


splitStrBy :: Char -> String -> [String]
splitStrBy _ [] = []
splitStrBy char str = 
  let (word, rest) = span (/= char) str
  in word : splitStrBy char (drop 1 rest)

splitSlash :: String -> [String]
splitSlash = splitStrBy '/'



exists :: String -> FTree String -> Bool
exists [] tree = True
exists _ FNil = False
exists str (FNode map) = 
  let keys = splitSlash str
  in case Map.lookup (head keys) map of
    Nothing -> False
    Just node -> 
      if null (tail keys) then True
      else exists (unwords (tail keys)) node



  -- let (key, rest) = span (/= '/') str
  -- in case Map.lookup key map of
  --   Nothing -> False
  --   Just node -> 
  --     if null rest then True
  --     else exists (drop 1 rest) node

