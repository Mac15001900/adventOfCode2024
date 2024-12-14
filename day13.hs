import           Data.Maybe ( catMaybes )
import           MUtils

type Point = (Rational, Rational)
type Target = Point
type Button = Point
type Machine = (Button, Button, Target)

parseMachine :: [String] -> Machine
parseMachine s = map (split (`elem` "+=,")) s |> map (\s' -> (s' !! 1, s' !! 3) |> mapBoth readInt) |> map (mapBoth fromIntegral) |> t3fromList

solve :: Machine -> Maybe Int
solve ((ax, ay), (bx, by), (tx, ty)) = if isNat a && isNat b then Just (3 * floor a + floor b) else Nothing
  where
    b = (ty - (tx * ay) / ax) / (by - (bx * ay) / ax)
    a = (tx - b * bx) / ax

isNat :: Rational -> Bool
isNat a = properFraction a |> snd |> (== 0)

part1 :: [String] -> Int
part1 s = splitOn "" s |> map parseMachine |> map solve |> catMaybes |> sum

part2 :: [String] -> Int
part2 s = splitOn "" s |> map parseMachine |> map (\(a, b, (tx, ty)) -> (a, b, (tx + 10000000000000, ty + 10000000000000))) |> map solve |> catMaybes |> sum

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
