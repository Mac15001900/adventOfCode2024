import           MUtils
import           GridUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Maybe (fromJust)

patrol :: Grid Char -> (Point, Dir) -> Set.Set Point -> Set.Set Point
patrol g (p, d) h
  | Map.notMember p g = h --We're outside the map, so we stop here
  | Map.lookup (stepDir d p) g == Just '#' = patrol g (p, rotateDirC d) h
  | otherwise = patrol g (stepDir d p, d) (Set.insert p h)

part1 :: [String] -> Int
part1 s = patrol (gridFromList s) start Set.empty |> Set.size
  where
    start = (gridFromList s |> flip getGridPosition '^' |> fromJust, North)


isLoop :: Grid Char -> (Point, Dir) -> Point -> Bool
isLoop g s p = isLoop' (Map.insert p '#' g) s Set.empty

isLoop' :: Grid Char -> (Point, Dir) -> Set.Set (Point, Dir) -> Bool
isLoop' g (p, d) h
  | Map.notMember p g = False
  | Set.member (p, d) h = True
  | Map.lookup (stepDir d p) g == Just '#' = isLoop' g (p, rotateDirC d) h
  | otherwise = isLoop' g (stepDir d p, d) (Set.insert (p, d) h)

part2 :: [String] -> Int
part2 s = candidates |> Set.toList |> count (isLoop (gridFromList s) start)
  where
    start = (gridFromList s |> flip getGridPosition '^' |> fromJust, North)
    candidates = patrol (gridFromList s) start Set.empty
      |> Set.delete (fst start)

test = [ "....#....."
       , ".........#"
       , ".........."
       , "..#......."
       , ".......#.."
       , ".........."
       , ".#..^....."
       , "........#."
       , "#........."
       , "......#..."]