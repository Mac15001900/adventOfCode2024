import           Data.List
import           Data.Maybe ( catMaybes )

import           GridUtils  ( Point, addPoints )

import           MUtils

type Target = Point

type Button = Point

type Machine = (Button, Button, Target)

--Cheks if a target is potentially rechable from a point (not all valid targets are reachable; all invalid ones aren't)
isValid :: Machine -> Point -> Bool
-- isValid ((dx1, dy1), (dx2, dy2), (tx, ty)) (px, py) = px <= tx && py <= ty && valueBetween ([ dy1 // dx1, dy2 // dx1 ] |> sort |> t2fromList) ((ty - py) // (tx - px)) --There's a mistake in here :/
isValid ((dx1, dy1), (dx2, dy2), (tx, ty)) (px, py) = px <= tx && py <= ty --This is less efficient but still works

--tryAStar :: Ord a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Maybe Int
stepsFrom :: Machine -> Point -> [(Point, Int)]
stepsFrom m@(b1, b2, t) p = [ (addPoints b1 p, 3), (addPoints b2 p, 1) ] |> filter (isValid m . fst)

heuristic :: Machine -> Point -> Int
heuristic ((dx1, dy1), (dx2, dy2), (tx, ty)) (px, py) = (tx - px + ty - py) `div` max ((dx1 + dy1) `div` 3 + 1) (dx2 + dy2)

parseMachine :: [String] -> Machine
parseMachine s = map (split (`elem` "+=,")) s |> map (\s' -> (s' !! 1, s' !! 3) |> mapBoth readInt) |> t3fromList

part1 :: [String] -> Int
part1 s = splitOn "" s |> map parseMachine |> map (\m -> tryAStar (stepsFrom m) (const 0) (0, 0) (== thd3 m)) |> catMaybes |> sum

test = [ "Button A: X+94, Y+34"
       , "Button B: X+22, Y+67"
       , "Prize: X=8400, Y=5400"
       , ""
       , "Button A: X+26, Y+66"
       , "Button B: X+67, Y+21"
       , "Prize: X=12748, Y=12176"
       , ""
       , "Button A: X+17, Y+86"
       , "Button B: X+84, Y+37"
       , "Prize: X=7870, Y=6450"
       , ""
       , "Button A: X+69, Y+23"
       , "Button B: X+27, Y+71"
       , "Prize: X=18641, Y=10279"
       ]