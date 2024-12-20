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

{-

--                 Track   Steps  Current  Explored         Frontier  
connectedWalls :: Track -> Int -> Point -> Set.Set Point -> [(Point, Int)] -> Set.Set Point
connectedWalls _ _ _ e [] = e --This could theoretically happen on smaller inputs
connectedWalls _ 0 p e _ = Set.insert p e --No steps to explore further
connectedWalls t s p e f = 

insertNodeBfs :: [(a, Int)] -> (a, Int) -> [(a, Int)]
insertNodeBfs [] node = [node]
insertNodeBfs ((a1,c1):xs) (a2,c2)
   | c2 < c1 = (a2,c2):(a1,c1):xs
   | otherwise =  (a1,c1):insertNodeBfs xs (a2,c2)
-}
type WallData = Map.Map Point (Map.Map Point Int)

wallsAllCosts :: Ord a => (a -> [(a, Int)]) -> a -> Map.Map a Int
wallsAllCosts neighbours start = wallsAllCosts' neighbours [ (start, 0) ] Set.empty Map.empty -- |> Map.toList

wallsAllCosts' :: Ord a => (a -> [(a, Int)]) -> [(a, Int)] -> Set.Set a -> Map.Map a Int -> Map.Map a Int
wallsAllCosts' n [] _ m = m
wallsAllCosts' n (next : frontier) visited m
    | Set.member (fst next) visited = wallsAllCosts' n frontier visited m
    | snd next > 18 = m
    | otherwise = wallsAllCosts' n newFrontier (Set.insert (fst next) visited) (uncurry Map.insert next m)
  where
    newFrontier = n (fst next) |> map (mapSnd (+ snd next)) |> filter ((`Set.notMember` visited) . fst) |> foldl insertNodeBfs frontier

insertNodeBfs :: [(a, Int)] -> (a, Int) -> [(a, Int)]
insertNodeBfs [] node = [ node ]
insertNodeBfs ((a1, c1) : xs) (a2, c2)
    | c2 < c1 = (a2, c2) : (a1, c1) : xs
    | otherwise = (a1, c1) : insertNodeBfs xs (a2, c2)

buildWallData :: Track -> WallData
buildWallData t = Map.keys t |> filter ((== '#') . (t Map.!)) |> map (\w -> (w, wallsAllCosts (map (`pair` 1) . neighbours t) w)) |> Map.fromList

cheats2 :: Track -> Distances -> WallData -> Point -> [Int]
cheats2 t d w p
    | t Map.! p == '#' = []
    | otherwise = neighbours t p |> filter ((== '#') . (t Map.!)) |> map (w Map.!) |> Map.unionsWith min |> Map.toList
        |> map (\(w', c) -> neighbours t w' |> filter ((/= '#') . (t Map.!)) |> map (`pair` (c + 2)) |> Map.fromList) |> Map.unionsWith min |> Map.toList
        |> map (\(p', c) -> (d Map.! p') + c) |> map ((d Map.! p) -)

part2 :: [String] -> Int
part2 s = let t = gridFromList s in indexes2 s |> map (cheats2 t (getDistances t) (buildWallData t)) |> concat |> count (>= 100)

partTest :: [String] -> [Int]
partTest s = let t = gridFromList s in indexes2 s |> map (cheats2 t (getDistances t) (buildWallData t)) |> concat |> filter (>= 50) |> sort

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

main = runOnFile "input20.txt" part2
--1000062 is too low