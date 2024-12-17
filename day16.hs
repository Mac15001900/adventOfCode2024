import           Data.List
import qualified Data.Map  as Map
import           GridUtils
import           MUtils

type Maze = Grid Char
type State = (Point, Dir)

getMoves :: Maze -> State -> [(State, Int)]
getMoves m (p, d) = [ (((p, rotateDirA d), 1000), stepDir (rotateDirA d) p), (((p, rotateDirC d), 1000), stepDir (rotateDirC d) p), (((stepDir d p, d), 1), stepDir d p) ]
    |> filter ((/= '#') . (m Map.!) . snd) |> map fst

part1 :: [String] -> Int
part1 s = let maze = gridFromList s in aStar (getMoves maze) (pointDistanceO (length (head s) - 2, 1) . fst) ((1, length s - 2), East) ((== (length (head s) - 2, 1)) . fst)

getMoves' :: Maze -> State -> [(State, Int)] --Returns moves that lead to the given state
getMoves' m (p, d) =
    [ (((p, rotateDirA d), 1000), stepDir (rotateDirC d) p), (((p, rotateDirC d), 1000), stepDir (rotateDirA d) p), (((stepDir (flipDir d) p, d), 1), stepDir (flipDir d) p) ]
    |> filter ((/= '#') . (m Map.!) . snd) |> map fst

part2 :: [String] -> Int
part2 s = Map.intersectionWith (+) reachCosts targetCosts |> Map.toList |> sortOn snd |> \res -> takeWhile ((== snd (head res)) . snd) res
    |> map (fst . fst) |> unique' |> length
  where
    maze = gridFromList s
    reachCosts = bfsAllCosts (getMoves maze) ((1, length s - 2), East)
    targetCosts = bfsAllCosts (getMoves' maze) ((length (head s) - 2, 1), East) -- A bit cheaty, other inputs could potentially have an ending where you don't face east

test = [ "###############"
       , "#.......#....E#"
       , "#.#.###.#.###.#"
       , "#.....#.#...#.#"
       , "#.###.#####.#.#"
       , "#.#.#.......#.#"
       , "#.#.#####.###.#"
       , "#...........#.#"
       , "###.#.#####.#.#"
       , "#...#.....#.#.#"
       , "#.#.#.###.#.#.#"
       , "#.....#...#.#.#"
       , "#.###.#.#.#.#.#"
       , "#S..#.....#...#"
       , "###############"
       ]