{-# LANGUAGE TupleSections #-}

import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           GridUtils

import           MUtils

type Gardens = NeighbourMap Int

type Costs = Map.Map Int (Int, Int) --For each region id: Area, perimeter

parse :: [String] -> (Gardens, Int)
parse s = map2 (, -1) s |> gridFromList |> neighbourMapO |> buildRegions (indexes2 s) 0

buildRegions :: [Point] -> Int -> NeighbourMap (Char, Int) -> (Gardens, Int)
buildRegions [] id g = (changeAllValues snd g, id - 1)
buildRegions (p : ps) id g
    | g Map.! p |> fst |> snd >= 0 = buildRegions ps id g
    | otherwise = getConnected g ((== letter) . fst) p |> foldr (changeValue (const (letter, id))) g |> buildRegions ps (id + 1)
  where
    letter = getValue g p |> fst

updateCosts :: Gardens -> Point -> Costs -> Costs
updateCosts g p = Map.adjust (addPoints (1, 4 - getNeighbours g p |> count ((== getValue g p) . snd))) (getValue g p)

part1 :: [String] -> Int
part1 s = foldr (updateCosts gardens) emptyCostMap (indexes2 s) |> Map.elems |> map (uncurry (*)) |> sum
  where
    (gardens, lastId) = parse s
    emptyCostMap = [ 0 .. lastId ] |> map (, (0, 0)) |> Map.fromList

--type Plot = (Int, Set.Set Dir) --Region id, directions that have been counted
type DirsHandled = Map.Map Point (Set.Set Dir) -- For each point the directions that are already handled

type State = (Costs, DirsHandled)

type Gardens2 = Grid Int

needsFence :: Gardens2 -> Point -> Dir -> Bool
needsFence g p d = Map.lookup (stepDir d p) g |> (Just (g Map.! p) /=)

updateCosts2 :: Gardens2 -> Point -> State -> State
updateCosts2 g p (costs, dirs) = (Map.adjust (addPoints (1, length dirsToUpdate)) id costs, newDirMap)
  where
    id = g Map.! p
    dirsToUpdate = allDirs |> filter (`Set.notMember` (dirs Map.! p)) |> filter (needsFence g p)
    -- dirsToUpdate = allDirs |> filter (needsFence g p)
    newDirMap = dirsToUpdate |> foldr (\dir -> updatePoints g p dir (rotateDirC dir) . updatePoints g p dir (rotateDirA dir)) dirs

updatePoints :: Gardens2 -> Point -> Dir -> Dir -> DirsHandled -> DirsHandled
updatePoints g p cd sd res
    | Map.notMember p g = res
    | otherwise =
        Map.adjust (Set.insert cd) p (if needsFence g (stepDir sd p) cd && Map.lookup p g == Map.lookup (stepDir sd p) g then updatePoints g (stepDir sd p) cd sd res else res)

part2 :: [String] -> Int
part2 s = foldr (updateCosts2 gardens) emptyState (indexes2 s) |> fst |> Map.elems |> map (uncurry (*)) |> sum
  where
    (gardens, lastId) = parse s |> mapFst gridFromNeighbourMap
    emptyState = ([ 0 .. lastId ] |> map (, (0, 0)) |> Map.fromList, indexes2 s |> map (, Set.empty) |> Map.fromList)

partTest :: [String] -> [(Int, Int)]
partTest s = foldr (updateCosts2 gardens) emptyState (indexes2 s) |> fst |> Map.elems
  where
    (gardens, lastId) = parse s |> mapFst gridFromNeighbourMap
    emptyState = ([ 0 .. lastId ] |> map (, (0, 0)) |> Map.fromList, indexes2 s |> map (, Set.empty) |> Map.fromList)

testO = [ "OOOOO", "OXOXO", "OOOOO", "OXOXO", "OOOOO" ]

test = [ "RRRRIICCFF", "RRRRIICCCF", "VVRRRCCFFF", "VVRCCCJFFF", "VVVVCJJCFE", "VVIVCCJJEE", "VVIIICJJEE", "MIIIIIJJEE", "MIIISIJEEE", "MMMISSJEEE" ]