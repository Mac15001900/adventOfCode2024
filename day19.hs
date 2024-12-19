import           MUtils

type Towel = String
type Pattern = String

parse :: [String] -> ([Towel], [Pattern])
parse s = splitOn "" s |> t2fromList |> mapFst (filter2 (/= ' ') . splitOn ',' . concat)

partialOptions :: [Towel] -> Pattern -> Either [Pattern] Integer
partialOptions ts [] = Right 1
partialOptions ts xs = filter ((<= length xs) . length) ts |> map (zip xs) |> filter (all (uncurry (==))) |> map length
    |> map (`drop` xs) |> \res -> if not (null res) then Left res else Right 0

part1 :: [String] -> Int
part1 s = let (towels, targets) = parse s in map (memoizedCount (partialOptions towels)) targets |> count (> 0)

part2 :: [String] -> Integer
part2 s = let (towels, targets) = parse s in map (memoizedCount (partialOptions towels)) targets |> sum

test = [ "r, wr, b, g, bwu, rb, gb, br", "", "brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb" ]
