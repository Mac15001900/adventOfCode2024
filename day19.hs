import           MUtils

type Towel = String

isPossible :: [Towel] -> String -> Towel -> Bool
isPossible p [] [] = True
isPossible p [] _ = False
isPossible p xs [] = any (isPossible p xs) p
isPossible p (x : xs) (t : ts)
    | x == t = isPossible p xs ts
    | otherwise = False

parse :: [String] -> ([Towel], [String])
parse s = splitOn "" s |> t2fromList |> mapFst (filter2 (/= ' ') . splitOn ',' . concat)

part1 :: [String] -> Int
part1 s = let (towels, targets) = parse s in count (\t -> any (isPossible towels t) towels) targets

countPossibilities :: [Towel] -> String -> Integer
countPossibilities ts = memoizedCount (partialOptions ts)

partialOptions :: [Towel] -> String -> Either [String] Integer
partialOptions ts [] = Right 1
partialOptions ts xs = filter ((<= length xs) . length) ts |> map (zip xs) |> filter (all (uncurry (==))) |> map length
    |> map (`drop` xs) |> \res -> if not (null res) then Left res else Right 0

part2 :: [String] -> Integer
part2 s = let (towels, targets) = parse s in map (countPossibilities towels) targets |> sum

test = [ "r, wr, b, g, bwu, rb, gb, br", "", "brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb" ]
