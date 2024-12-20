import           Data.List  ( sort )
import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set

import           GridUtils

import           MUtils

type Track = Grid Char

type Distances = Map.Map Point Int

getDistances :: Track -> Distances
getDistances t = bfsAllCosts (\p -> directionsO |> map (addPoints p) |> filter ((/= '#') . (t Map.!)) |> map (`pair` 1)) (getGridPosition t 'E' |> fromJust)

getCheats :: Track -> Distances -> Point -> [Int]
getCheats t d p
    | t Map.! p == '#' = []
    | otherwise = directionsO |> map (addPoints p) |> filter ((== Just '#') . (`Map.lookup` t))
        |> map (\w -> directionsO |> map (addPoints w) |> filter ((\c -> isJust c && c /= Just '#') . (`Map.lookup` t)))
        |> map2 (`Map.lookup` d) |> concat |> catMaybes |> map (+ 2) |> map ((d Map.! p) -) |> filter (> 0)

part1 :: [String] -> Int
part1 s = let t = gridFromList s in indexes2 s |> map (getCheats t (getDistances t)) |> concat |> count (>= 100)

neighbours :: Track -> Point -> [Point]
neighbours t p = directionsO |> map (addPoints p) |> filter (`Map.member` t)

getCheats2 :: Track -> Distances -> Point -> [Int]
getCheats2 t d p
    | t Map.! p == '#' = []
    | otherwise = allPointsInRangeO t 20 p |> filter ((/= '#') . (t Map.!)) |> map (\p' -> (d Map.! p') + pointDistanceO p p')
        |> map ((d Map.! p) -) |> filter (> 0)

part2 :: [String] -> Int
part2 s = let t = gridFromList s in indexes2 s |> map (getCheats2 t (getDistances t)) |> concat |> count (>= 100)

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
