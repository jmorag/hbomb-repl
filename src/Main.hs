import RIO
import RIO.Char
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          outputStrLn $ "Input was: " ++ (process input)
          loop

process :: [Char] -> [Char]
process input = map toUpper input
