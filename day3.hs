import           MUtils
import           Data.Char

process :: String -> Int
process [] = 0
process ('m':'u':'l':'(':xs) = case readNumber xs 0 True of
  (Just n, xs')  -> n + (process xs')
  (Nothing, xs') -> process xs'
process (_:xs) = process xs

--String to parse, accumulator, is it the first number
readNumber :: String -> Int -> Bool -> (Maybe Int, String)
readNumber [] n f = (Nothing, [])
readNumber (',':xs) n True = case readNumber xs 0 False of
  (Just n', xs') -> (Just (n * n'), xs')
  (Nothing, xs') -> (Nothing, xs)
readNumber (')':xs) n False = (Just n, xs)
readNumber (x:xs) n f
  | isDigit x = readNumber xs (n * 10 + (read [x])) f
  | otherwise = (Nothing, x:xs)

part1 :: [String] -> Int
part1 s = map process s |> sum

process2 :: String -> Bool -> Int
process2 [] _ = 0
process2 ('d':'o':'(':')':xs) _ = process2 xs True
process2 ('d':'o':'n':'\'':'t':'(':')':xs) _ = process2 xs False
process2 ('m':'u':'l':'(':xs) True = case readNumber xs 0 True of
  (Just n, xs')  -> n + (process2 xs' True)
  (Nothing, xs') -> process2 xs True
process2 (_:xs) e = process2 xs e

part2 :: [String] -> Int
part2 s = joinWith "x" s |> (`process2` True)

test =
  ["xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"]

test2 =
  ["xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]
--107519039 is too high