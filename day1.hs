import           MUtils
import           Data.List

parse :: [String] -> ([Int], [Int])
parse s = s
  |> map (splitOn ' ')
  |> map (\l -> (readInt (head l), readInt (last l)))
  |> unzip

part1 :: [String] -> Int
part1 s =
  parse s |> mapBoth sort |> uncurry zip |> map (\(a, b) -> abs (a - b)) |> sum

part2 :: [String] -> Int
part2 s = map (\a -> a * count (== a) bs) as |> sum
  where
    (as, bs) = parse s