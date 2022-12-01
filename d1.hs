module Main where
import Data.List (sort)


main :: IO ()
main = do
  d1 <- readFile "d1_input.txt"
  let sorted = reverse . sort $ parseInp d1
  print (d1p1 d1)
  print (d1p2 sorted)

d1p1 :: String -> Int
d1p1 = fst . foldl d1p1' (0, 0) . lines

d1p1' :: (Int, Int) -> String -> (Int, Int)
d1p1' (maxn, count) "" = (maxn, 0)
d1p1' (maxn, count) num | count + n > maxn = (count + n, count + n)
                      | otherwise = (maxn, count + n)
  where n = read num


d1p2 = sum . take 3

split' :: ([[Int]], [Int]) -> String -> ([[Int]], [Int])
split' (ls, c) "" = (c:ls, [])
split' (ls, c) n = (ls, c ++ [read n])

split :: [String] -> [[Int]]
split = (\(x, y) -> y:x) . foldl split' ([], [])

parseInp :: String -> [Int] -- parse strings to list of ints, where each int is an elf's calories
parseInp = fmap sum . split . lines