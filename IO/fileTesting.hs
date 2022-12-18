import Data.List

readAndWrite :: String -> String -> IO String
readAndWrite fn s = writeFile fn s >> readFile fn

reading :: String -> IO ()
reading fn = readFile fn >>= print

--makeList :: String -> [String]
--makeList fn = lines (readFile fn)

--makeList fn = splitAt "\n" readFile fn

main :: IO ()
main = do
    --readAndWrite "a.txt" "a" >>= putStrLn
    reading "text.txt"
    --makeList "text.txt" >>= print