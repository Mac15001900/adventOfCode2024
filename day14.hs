{-# LANGUAGE TupleSections #-}

import           Data.Bifunctor ( Bifunctor(bimap) )
import qualified Data.Set       as Set

import           GridUtils      ( Point, addPoints )

import           MUtils

type Robot = (Point, Point) -- Position, velocity

parse :: String -> Robot
parse s = map (split (`elem` "=, ") s !!) [ 1, 2, 4, 5 ] |> map readInt |> \[x, y, dx, dy] -> ((x, y), (dx, dy))

move :: (Int, Int) -> Int -> Robot -> Robot
move (w, h) steps ((x, y), (dx, dy)) = (((x + dx * steps) `mod` w, (y + dy * steps) `mod` h), (dx, dy))

quadrant :: (Int, Int) -> Robot -> Int
quadrant (w, h) ((x, y), _) = case (left, right, up, down) of
    (True, _, True, _) -> 1
    (_, True, True, _) -> 2
    (True, _, _, True) -> 3
    (_, True, _, True) -> 4
    _ -> 0
  where
    left = x < (w - 1) `div` 2
    right = x > (w - 1) `div` 2
    up = y < (h - 1) `div` 2
    down = y > (h - 1) `div` 2

part1 :: [String] -> Int
part1 s = map parse s |> map (move (101, 103) 100) |> map (\r -> (r, quadrant (101, 103) r)) |> \rb -> map (\q -> count ((== q) . snd) rb) [ 1 .. 4 ] |> product

--Counts how many robots are next to other robots
distanceScore :: [Robot] -> Int
distanceScore rb = map fst rb |> count (\r -> directions2D |> map (addPoints r) |> any (`Set.member` taken))
  where
    taken = map fst rb |> Set.fromList

showRobots :: [Robot] -> String
showRobots rb = combinations [ 0 .. 100 ] [ 0 .. 102 ] |> map (`Set.member` taken) |> map (\r -> if r then 'X' else '_') |> groupInto2D 101 |> joinWith "\n"
  where
    taken = map fst rb |> Set.fromList

--Will search all steps from a given one, until it finds a state with a high enough distance score. It will then print the grid, step count and grid's distance score
searchFrom :: Int -> Int -> [String] -> IO ()
searchFrom start threshold s = map parse s |> map (move (101, 103) start) |> (, start) |> repeatUntil ((> threshold) . distanceScore . fst) (bimap (map (move (101, 103) 1)) (+ 1))
    |> \(rs, v) -> (showRobots rs, show v, show $ distanceScore rs) |> t3toList |> joinWith "\n\n" |> putStrLn

test = [ "p=0,4 v=3,-3"
       , "p=6,3 v=-1,-3"
       , "p=10,3 v=-1,2"
       , "p=2,0 v=2,-1"
       , "p=0,0 v=1,3"
       , "p=3,0 v=-2,-2"
       , "p=7,6 v=-1,-3"
       , "p=3,0 v=-1,-2"
       , "p=9,3 v=2,3"
       , "p=7,3 v=-1,2"
       , "p=2,4 v=2,-3"
       , "p=9,5 v=-3,-3"
       ]