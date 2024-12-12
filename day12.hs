{-# LANGUAGE TupleSections #-}

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           GridUtils
import           MUtils

type Gardens = Grid Int
type Costs = Map.Map Int (Int, Int) --For each region id: Area, perimeter
type DirsHandled = Map.Map Point (Set.Set Dir) -- For each point the directions that are already handled
type State = (Costs, DirsHandled)

parse :: [String] -> (Gardens, Int)
parse s = map2 (, -1) s |> gridFromList |> neighbourMapO |> buildRegions (indexes2 s) 0

buildRegions :: [Point] -> Int -> NeighbourMap (Char, Int) -> (Gardens, Int)
buildRegions [] id g = (gridFromNeighbourMap g |> Map.map snd, id - 1)
buildRegions (p : ps) id g
    | g Map.! p |> fst |> snd >= 0 = buildRegions ps id g
    | otherwise = getConnected g ((== letter) . fst) p |> foldr (changeValue (const (letter, id))) g |> buildRegions ps (id + 1)
  where
    letter = getValue g p |> fst

needsFence :: Gardens -> Point -> Dir -> Bool
needsFence g p d = Map.lookup (stepDir d p) g |> (Just (g Map.! p) /=)

updateCosts :: Bool -> Gardens -> Point -> State -> State
updateCosts discount g p (costs, dirs) = (Map.adjust (addPoints (1, length dirsToUpdate)) id costs, newDirMap)
  where
    id = g Map.! p
    dirsToUpdate = allDirs |> filter ((not discount ||) . (`Set.notMember` (dirs Map.! p))) |> filter (needsFence g p)
    newDirMap = dirsToUpdate |> foldr (\dir -> updatePoints g p dir (rotateDirC dir) . updatePoints g p dir (rotateDirA dir)) dirs

updatePoints :: Gardens -> Point -> Dir -> Dir -> DirsHandled -> DirsHandled
updatePoints g p cd sd res --cs is the direction we're checking, sd is the direction we're stepping towards
    | Map.notMember p g = res
    | otherwise = Map.adjust (Set.insert cd) p (if keepGoing then updatePoints g (stepDir sd p) cd sd res else res)
  where
    keepGoing = needsFence g (stepDir sd p) cd && Map.lookup p g == Map.lookup (stepDir sd p) g

solve :: Bool -> [String] -> Int
solve discount s = foldr (updateCosts discount gardens) emptyState (indexes2 s) |> fst |> Map.elems |> map (uncurry (*)) |> sum
  where
    (gardens, lastId) = parse s
    emptyState = ([ 0 .. lastId ] |> map (, (0, 0)) |> Map.fromList, indexes2 s |> map (, Set.empty) |> Map.fromList)

part1 :: [String] -> Int
part1 = solve False

part2 :: [String] -> Int
part2 = solve True

testO = [ "OOOOO", "OXOXO", "OOOOO", "OXOXO", "OOOOO" ]

test = [ "RRRRIICCFF", "RRRRIICCCF", "VVRRRCCFFF", "VVRCCCJFFF", "VVVVCJJCFE", "VVIVCCJJEE", "VVIIICJJEE", "MIIIIIJJEE", "MIIISIJEEE", "MMMISSJEEE" ]