import Hw4

main :: IO ()
main = do inp <- getContents
          case readPrg inp of
            Nothing -> putStrLn "Incorrect program"
            Just e -> print $ e

