import Data.List

inputWords :: IO String
inputWords = readFile "text.txt"

main :: IO ()
main = do
    print(lines (readFile "test.txt"))
    