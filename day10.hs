import           Data.Char
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           GridUtils

import           MUtils

type Area = NeighbourMap Int

validSteps :: Area -> Point -> [Point]
validSteps a p = let (h, ns) = a Map.! p in filter ((== h + 1) . snd) ns |> map fst

reachable :: Area -> Point -> Set.Set Point
reachable a p
    | a Map.! p |> fst == 9 = Set.singleton p
    | otherwise = validSteps a p |> map (reachable a) |> Set.unions

solve :: (Area -> Point -> Int) -> [String] -> Int
solve f s = zipWithIndexes2 s |> filter2 ((== '0') . fst) |> map2 snd |> concat |> map (f area) |> sum
  where
    area = map2 digitToInt s |> gridFromList |> neighbourMapO

part1 :: [String] -> Int
part1 = solve (\a p -> reachable a p |> Set.size)

rating :: Area -> Point -> Int
rating a p
    | a Map.! p |> fst == 9 = 1
    | otherwise = validSteps a p |> map (rating a) |> sum

part2 :: [String] -> Int
part2 = solve rating

test = [ "89010123", "78121874", "87430965", "96549874", "45678903", "32019012", "01329801", "10456732" ]