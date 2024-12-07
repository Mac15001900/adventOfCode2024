import           MUtils

type Equation = (Integer, [Integer])

parse :: String -> Equation
parse s =
  splitOn ':' s |> t2fromList |> mapFst read |> mapSnd (map read . words)

solvable :: Equation -> Integer -> Bool
solvable (r, []) acc = r == acc
solvable (r, x:xs) acc = solvable (r, xs) (acc + x)
  || solvable (r, xs) (acc * x)

part1 :: [String] -> Integer
part1 s = map parse s |> filter (`solvable` 0) |> map fst |> sum

concatOp :: Integer -> Integer -> Integer
concatOp a b = (show a ++ show b) |> read

solvable2 :: Equation -> Integer -> Bool
solvable2 (r, []) acc = r == acc
solvable2 (r, x:xs) acc = solvable2 (r, xs) (acc + x)
  || solvable2 (r, xs) (acc * x)
  || solvable2 (r, xs) (concatOp acc x)

part2 :: [String] -> Integer
part2 s = map parse s |> filter (`solvable2` 0) |> map fst |> sum

test = [ "190: 10 19"
       , "3267: 81 40 27"
       , "83: 17 5"
       , "156: 15 6"
       , "7290: 6 8 6 15"
       , "161011: 16 10 13"
       , "192: 17 8 14"
       , "21037: 9 7 18 13"
       , "292: 11 6 16 20"]