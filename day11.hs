{-# LANGUAGE TupleSections #-}

import           Data.List

import           MUtils

type StoneState = (Integer, Integer) --Stone value, simulation steps left

simulate :: StoneState -> Either [StoneState] Integer
simulate (s, 0) = Right 1
simulate (0, n) = Left [ (1, n - 1) ]
simulate (s, n) = case splitNumber s of
    Just (l, r) -> Left [ (l, n - 1), (r, n - 1) ]
    Nothing -> Left [ (s * 2024, n - 1) ]

splitNumber :: Integer -> Maybe (Integer, Integer)
splitNumber n = let s = show n in if even (length s) then genericSplitAt (length s `div` 2) s |> mapBoth read |> Just else Nothing

solve :: Integer -> [String] -> Integer
solve n s = head s |> words |> map read |> map (, n) |> map (memoizedCount simulate) |> sum

part1 :: [String] -> Integer
part1 = solve 25

part2 :: [String] -> Integer
part2 = solve 75

test = [ "125 17" ]