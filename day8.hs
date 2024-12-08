import           MUtils
import qualified Data.Map  as Map
import           GridUtils
import qualified Data.Set  as Set

getNodes :: [String] -> Char -> [Point]
getNodes s c = zipWithIndexes2 s |> filter2 ((== c) . fst) |> map2 snd |> concat

antinodes :: Point -> Point -> [Point]
antinodes (x1, y1) (x2, y2) = [ (x1 * 2 - x2, y1 * 2 - y2), (x2 * 2 - x1, y2 * 2 - y1) ]

solve :: (Point -> Point -> [Point]) -> [String] -> Int
solve f s = unique' (concat s) |> filter (/= '.') |> map (\t -> getNodes s t |> combinationsSelf |> map (uncurry f) |> concat |> filter (`Map.member` grid)) 
    |> map Set.fromList |> Set.unions |> Set.size
  where
    grid = gridFromList s

part1 :: [String] -> Int
part1 = solve antinodes

superAntinodes :: Int -> Point -> Point -> [Point]
superAntinodes n (x1, y1) (x2, y2) = let diff = (x1 - x2, y1 - y2) in map (`mulPoint` diff) [ -n .. n ] |> map (addPoints (x2, y2))

part2 :: [String] -> Int
part2 s = solve (superAntinodes (max (length s) (length $ head s))) s

test = [ "............"
       , "........0..."
       , ".....0......"
       , ".......0...."
       , "....0......."
       , "......A....."
       , "............"
       , "............"
       , "........A..."
       , ".........A.."
       , "............"
       , "............"
       ]