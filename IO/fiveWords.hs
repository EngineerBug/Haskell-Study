import Data.List

--read in the file

--format the input

--carry out the calculations

--write to the output file


inputWords :: IO ()
inputWords = readFile "text.txt" >>= print

main :: IO ()
main = do
    inputWords
    