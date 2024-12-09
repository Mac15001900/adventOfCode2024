import           Data.Char  hiding ( Space )
import           Data.List
import           Data.Maybe

--import           Debug.Trace
import           MUtils

type Disc = [Maybe Int] --Each cell is either empty space (Nothing) or the id of the file on it (Just Int)

uncompress :: [Int] -> Int -> Disc
uncompress [] _ = []
uncompress [f] n = replicate f (Just n)
uncompress (f : s : xs) n = replicate f (Just n) ++ replicate s Nothing ++ uncompress xs (n + 1)

organise :: Disc -> Disc
organise d = case last d of
    Just n -> maybe d organise (replaceFist Nothing (Just n) (init d))
    Nothing -> organise $ init d

--Replaces the first instance of the first argument with the second argument. Returns Nothing if none were found.
replaceFist :: Eq a => a -> a -> [a] -> Maybe [a]
replaceFist a b [] = Nothing
replaceFist a b (x : xs)
    | a == x = Just (b : xs)
    | otherwise = (x :) <$> replaceFist a b xs

part1 :: [String] -> Int
part1 s = head s |> map digitToInt |> (`uncompress` 0) |> organise |> catMaybes |> zipWithIndexes |> map (uncurry (*)) |> sum

showDisc :: Disc -> String
showDisc [] = []
showDisc (Nothing : xs) = '.' : showDisc xs
showDisc ((Just x) : xs) = show x ++ showDisc xs

---- Part 2 ----
data Content = File Int Int | Space Int --File id size | Space size
    deriving ( Show, Read, Eq )

type Dics2 = [Content]

parse :: [Int] -> Int -> Dics2
parse [] _ = []
parse [f] n = [ File n f ]
parse (f : s : xs) n = File n f : Space s : parse xs (n + 1)

organise2 :: Dics2 -> Int -> Dics2
--organise2 d i    | traceShow (d |> uncompress2 |> showDisc) False = error ""
organise2 d 0 = d
organise2 d n
    | find ((== n) . getId) d |> isNothing = error $ "Failed to find file with id " ++ show n ++ " on disc " ++ show d
organise2 d n = find ((== n) . getId) d |> fromJust |> moveFile d |> fromMaybe d |> (`organise2` (n - 1))

getId :: Content -> Int
getId (Space _) = -1
getId (File id _) = id

moveFile :: Dics2 -> Content -> Maybe Dics2
moveFile [] _ = Nothing
moveFile (x : xs) f
    | x == f = Nothing
moveFile (x@(File _ _) : xs) f = (x :) <$> moveFile xs f
moveFile (Space ns : xs) f@(File _ nf)
    | nf <= ns = Just (Space 0 : f : Space (ns - nf) : removeFile xs f)
    | otherwise = (Space ns :) <$> moveFile xs f

removeFile :: Dics2 -> Content -> Dics2
removeFile [] c = error $ "File not found" ++ show c
removeFile (x@(File _ n) : xs) f
    | x == f = Space n : xs
    | otherwise = x : removeFile xs f
removeFile (Space n1 : Space n2 : xs) f = removeFile (Space (n1 + n2) : xs) f
removeFile (Space n1 : x : Space n2 : xs) f@(File _ nf)
    | x == f = Space (n1 + nf + n2) : xs
    | otherwise = Space n1 : x : removeFile (Space n2 : xs) f
removeFile xs _ = error $ "No pattern matched in removeFile for " ++ show xs

--removeFile d _ = d
uncompress2 :: Dics2 -> [Maybe Int]
uncompress2 [] = []
uncompress2 (Space n : xs) = replicate n Nothing ++ uncompress2 xs
uncompress2 (File id n : xs) = replicate n (Just id) ++ uncompress2 xs

part2 :: [String] -> Int
part2 s = organise2 (disc ++ [ Space 0 ]) (last disc |> getId) |> uncompress2 |> map (fromMaybe 0) |> zipWithIndexes
    |> map (uncurry (*)) |> sum
  where
    disc = head s |> map digitToInt |> (`parse` 0)

test = [ "2333133121414131402" ]
