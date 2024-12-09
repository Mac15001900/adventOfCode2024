import           Data.Char  hiding ( Space )
import           Data.List
import           Data.Maybe

import           MUtils

---- Part 1 ----
type Disc = [Maybe Int] --Each cell is either empty space (Nothing) or the id of the file on it (Just Int)

uncompress :: [Int] -> Int -> Disc
uncompress [] _ = []
uncompress [f] n = replicate f (Just n)
uncompress (f : s : xs) n = replicate f (Just n) ++ replicate s Nothing ++ uncompress xs (n + 1)

organise :: Disc -> Disc
organise d = case last d of
    Just n -> maybe d organise (replaceFirst Nothing (Just n) (init d))
    Nothing -> organise $ init d

--Replaces the first instance of the first argument with the second argument. Returns Nothing if none were found.
replaceFirst :: Eq a => a -> a -> [a] -> Maybe [a]
replaceFirst a b [] = Nothing
replaceFirst a b (x : xs)
    | a == x = Just (b : xs)
    | otherwise = (x :) <$> replaceFirst a b xs

part1 :: [String] -> Int
part1 s = head s |> map digitToInt |> (`uncompress` 0) |> organise |> catMaybes |> zipWithIndexes |> map (uncurry (*)) |> sum

---- Part 2 ----
data Content = File Int Int | Space Int --File id size | Space size
    deriving ( Show, Read, Eq )

type Disc2 = [Content]

parse :: [Int] -> Int -> Disc2
parse [] _ = []
parse [f] n = [ File n f ]
parse (f : s : xs) n = File n f : Space s : parse xs (n + 1)

organise2 :: Disc2 -> Int -> Disc2
organise2 d 0 = d
organise2 d n = find ((== n) . getId) d |> fromJust |> moveFile d |> fromMaybe d |> (`organise2` (n - 1))

getId :: Content -> Int
getId (Space _) = -1
getId (File id _) = id

moveFile :: Disc2 -> Content -> Maybe Disc2
moveFile [] _ = Nothing
moveFile (Space n1 : Space n2 : xs) f = moveFile (Space (n1 + n2) : xs) f
moveFile (x@(File _ _) : xs) f
    | x == f = Nothing --If we're here, there was no space on the left on the file
    | otherwise = (x :) <$> moveFile xs f
moveFile (Space ns : xs) f@(File _ nf)
    | nf <= ns = Just (f : Space (ns - nf) : (replaceFirst f (Space nf) xs |> fromJust))
    | otherwise = (Space ns :) <$> moveFile xs f

uncompress2 :: Disc2 -> [Maybe Int]
uncompress2 [] = []
uncompress2 (Space n : xs) = replicate n Nothing ++ uncompress2 xs
uncompress2 (File id n : xs) = replicate n (Just id) ++ uncompress2 xs

part2 :: [String] -> Int
part2 s = organise2 disc (last disc |> getId) |> uncompress2 |> map (fromMaybe 0) |> zipWithIndexes |> map (uncurry (*)) |> sum
  where
    disc = head s |> map digitToInt |> (`parse` 0)

---- Debug ----
showDisc :: Disc -> String
showDisc [] = []
showDisc (Nothing : xs) = '.' : showDisc xs
showDisc ((Just x) : xs) = show x ++ showDisc xs

test = [ "2333133121414131402" ]
--Part 1: 6415184586041
--Part 2: 6436819084274