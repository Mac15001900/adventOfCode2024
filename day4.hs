import           MUtils
import           GridUtils
import qualified Data.Map as Map
import           Data.List

xmasCount :: Grid Char -> (Int, Int) -> Int
xmasCount g (w, h) = combinations [0 .. w - 1] [0 .. h - 1]
  |> map
    (\p -> map2 (addPoints p) offsets
     |> map2 (`Map.lookup` g)
     |> map removeNothing
     |> count (== "XMAS"))
  |> sum

offsets :: [[(Int, Int)]]
offsets = directions2D
  |> map (zip [0 .. 3] . repeat)
  |> map2 (\(v, (x, y)) -> (x * v, y * v))

part1 :: [String] -> Int
part1 s = xmasCount (gridFromList s) (length (head s), length s)

checkA :: Grid Char -> (Int, Int) -> Bool
checkA g p = g Map.! p == 'A'
  && checkPoints [(-1, -1), (1, 1)]
  && checkPoints [(-1, 1), (1, -1)]
  where
    checkPoints list = map (addPoints p) list
      |> map (`Map.lookup` g)
      |> removeNothing
      |> sort
      |> (== "MS")

part2 :: [String] -> Int
part2 s = indexes2 s |> count (checkA (gridFromList s))

test = [ "MMMSXXMASM"
       , "MSAMXMSMSA"
       , "AMXSXMAAMM"
       , "MSAMASMSMX"
       , "XMASAMXAMM"
       , "XXAMMXXAMA"
       , "SMSMSASXSS"
       , "SAXAMASAAA"
       , "MAMMMXMMMM"
       , "MXMXAXMASX"]