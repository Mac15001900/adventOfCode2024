import           MUtils
import           Data.List
import           Data.Maybe

type Rule = (Int, Int)

type Update = [Int]

parse :: [String] -> ([Rule], [Update])
parse s = splitOn "" s
  |> t2fromList
  |> mapFst (map (\line -> splitOn '|' line |> t2fromList |> mapBoth read))
  |> mapSnd (map (\line -> splitOn ',' line |> map read))

ruleApplies :: Update -> Rule -> Bool
ruleApplies [] _ = True
ruleApplies (x:xs) (a, b)
  | x == b = a `notElem` xs
  | otherwise = ruleApplies xs (a, b)

midValue :: Update -> Int
midValue xs = xs !! (length xs `div` 2)

part1 :: [String] -> Int
part1 s =
  let (rs, us) = parse s
  in filter (\u -> map (ruleApplies u) rs |> and) us |> map midValue |> sum

fixUpdate :: [Rule] -> Update -> Maybe Update
fixUpdate rs u = case find (not . ruleApplies u) rs of
  Just r  -> swapElements r u |> \u' -> Just (fromMaybe u' (fixUpdate rs u'))
  Nothing -> Nothing

swapElements :: Eq a => (a, a) -> [a] -> [a]
swapElements r@(a, b) xs = xs |> setElement i1 b |> setElement i2 a
  where
    (i1, i2) = mapBoth (fromJust . (`elemIndex` xs)) r

part2 :: [String] -> Int
part2 s = let (rs, us) = parse s
          in map (fixUpdate rs) us |> catMaybes |> map midValue |> sum

test =
  [ "47|53"
  , "97|13"
  , "97|61"
  , "97|47"
  , "75|29"
  , "61|13"
  , "75|53"
  , "29|13"
  , "97|29"
  , "53|29"
  , "61|53"
  , "97|53"
  , "61|29"
  , "47|13"
  , "75|47"
  , "97|75"
  , "47|61"
  , "75|61"
  , "47|29"
  , "75|13"
  , "53|13"
  , ""
  , "75,47,61,53,29"
  , "97,61,53,29,13"
  , "75,29,13"
  , "75,97,47,61,53"
  , "61,13,29"
  , "97,13,75,29,47"]