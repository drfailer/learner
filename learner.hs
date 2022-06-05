import Control.Monad
import Data.List
import Data.List.Split
import System.Environment
import System.IO

separator :: String
separator = "|"

red :: String
red = "\x1b[31m"

green :: String
green = "\x1b[32m"

reset :: String
reset = "\x1b[0m"

bold :: String
bold = "\x1b[30;1m"

parseFile :: [String] -> [(String, String)]
parseFile lns = [parseLine l | l <- lns]
  where
    parseLine s =
      let parts = splitOn separator s
       in (parts !! 0, parts !! 1)

askWord :: String -> IO String
askWord w = do
  putStrLn $ "Translate: " ++ w
  putStr $ bold ++  "#voc-learner> " ++ reset
  hFlush stdout
  answer <- getLine
  return answer

validateAns :: String -> String -> Bool
validateAns ans sol = (unwords eltsAns) == (unwords eltsSol)
  where
    eltsAns = filter (\x -> x /= "") $ splitOneOf "/, " ans
    eltsSol = filter (\x -> x /= "") $ splitOneOf "/, " sol

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ head args
  let vocab = parseFile $ lines content
      total = length vocab
  rights <-
    forM vocab $ \w -> do
      answer <- askWord $ fst w
      if validateAns answer (snd w)
        then putStrLn (green ++ answer ++ reset ++ " is a good answer.\n") >>
             return 1
        else putStrLn
               ("bad expected: " ++ red ++ (snd w) ++ reset ++ "\n") >>
             return 0
  let score = sum rights
      percentage = 100 * (fromIntegral score) / (fromIntegral total)
  putStrLn $
    "Your score is: " ++
    (colorize percentage) ++ (show percentage) ++ "%." ++ reset
  where
    colorize percentage =
      if percentage > 50.0
        then green
        else red
