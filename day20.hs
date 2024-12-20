import qualified Data.Map   as Map
import           Data.Maybe
import           GridUtils
import           MUtils

type Track = Grid Char
type Distances = Map.Map Point Int

getDistances :: Track -> Distances
getDistances t = bfsAllCosts (\p -> directionsO |> map (addPoints p) |> filter ((/= '#') . (t Map.!)) |> map (`pair` 1)) (getGridPosition t 'E' |> fromJust)

cheats :: Int -> Track -> Distances -> Point -> [Int]
cheats r t d p
    | t Map.! p == '#' = []
    | otherwise = allPointsInRangeO t r p |> filter ((/= '#') . (t Map.!)) |> map (\p' -> (d Map.! p') + pointDistanceO p p')
        |> map ((d Map.! p) -) |> filter (> 0)

solve :: Int -> [String] -> Int
solve r s = let t = gridFromList s in indexes2 s |> map (cheats r t (getDistances t)) |> concat |> count (>= 100)

part1 :: [String] -> Int
part1 = solve 2

part2 :: [String] -> Int
part2 = solve 20

test = [ "###############"
       , "#...#...#.....#"
       , "#.#.#.#.#.###.#"
       , "#S#...#.#.#...#"
       , "#######.#.#.###"
       , "#######.#.#...#"
       , "#######.#.###.#"
       , "###..E#...#...#"
       , "###.#######.###"
       , "#...###...#...#"
       , "#.#####.#.###.#"
       , "#.#...#.#.#...#"
       , "#.#.#.#.#.#.###"
       , "#...#...#...###"
       , "###############"
       ]
