import           MUtils

parse :: [String] -> [[Int]]
parse s = map words s |> map2 read

isSafe :: [Int] -> Bool
isSafe r = (differences r |> map abs |> all (<= 3))
  && (differences r |> all (> 0) || differences r |> all (< 0))

part1 :: [String] -> Int
part1 s = parse s |> count isSafe

isSafe2 :: [Int] -> Bool
isSafe2 r = indexes r |> map (`removeElement` r) |> any isSafe

part2 :: [String] -> Int
part2 s = parse s |> count isSafe2

test :: [String]
test = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
  |> splitOn '\n'
--416 is too low, 445 is too high