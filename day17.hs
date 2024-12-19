import           Data.Bifunctor ( Bifunctor(bimap) )
import           Data.Bits      ( xor )
import qualified Data.Map       as Map
import           Data.Tuple     ( swap )

import           MUtils

type Instruction = (Int, Int)
type Program = Map.Map Int Int
type Registers = (Int, Int, Int)
type State = (Int, Registers, [Int]) --Next instruction to execute, registers, outputs

execute :: Program -> State -> State
execute p s@(i, rs@(ra, rb, rc), o) = case Map.lookup i p of
    Nothing -> s
    Just 0 -> execute p (i + 2, (ra `div` (2 ^ cop), rb, rc), o)
    Just 1 -> execute p (i + 2, (ra, xor rb lop, rc), o)
    Just 2 -> execute p (i + 2, (ra, cop `mod` 8, rc), o)
    Just 3 -> execute p (if ra == 0 then i + 2 else lop, (ra, rb, rc), o)
    Just 4 -> execute p (i + 2, (ra, xor rb rc, rc), o)
    Just 5 -> execute p (i + 2, (ra, rb, rc), (cop `mod` 8) : o)
    Just 6 -> execute p (i + 2, (ra, ra `div` (2 ^ cop), rc), o)
    Just 7 -> execute p (i + 2, (ra, rb, ra `div` (2 ^ cop)), o)
  where
    lop = p Map.! (i + 1)
    cop = if lop <= 3 then lop else t3toList rs !! (lop - 4)

parse :: [String] -> (Program, State)
parse s = splitOn "" s |> map2 (last . splitOn ':') |> t2fromList |> bimap (t3fromList . map readInt) (map readInt . splitOn ',' . head) |> \(rs, p) ->
    (Map.fromList (zipWithIndexes p |> map swap), (0, rs, []))

part1 :: [String] -> String
part1 s = parse s |> uncurry execute |> thd3 |> reverse |> show |> tail |> init

findPossibilities :: [Int] -> Int -> [Int]
findPossibilities [] a = [ a ]
findPossibilities (x : xs) a = [ 0 .. 7 ] |> filter (\a8 -> x == (((a * 8 + a8) `div` (2 ^ (a8 `xor` 1))) `xor` 5 `xor` a8) `mod` 8)
    |> map (+ a * 8) |> map (findPossibilities xs) |> concat

part2 :: [String] -> Int --This will only work on my specific input; a general solution might require solving the Halting Problem
part2 s = last s |> splitOn ':' |> last |> splitOn ',' |> map readInt |> reverse |> (`findPossibilities` 0) |> minimum

test = [ "Register A: 729", "Register B: 0", "Register C: 0", "", "Program: 0,1,5,4,3,0" ]

test2 = [ "Register A: 2024", "Register B: 0", "Register C: 0", "", "Program: 0,3,5,4,3,0" ]