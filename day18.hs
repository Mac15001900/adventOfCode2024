import           Data.Maybe ( isNothing )
import qualified Data.Set   as Set
import           GridUtils
import           MUtils

inBounds :: Point -> Bool
inBounds (x, y) = all (valueBetween (0, 70)) [ x, y ]

part1 :: [String] -> Int
part1 s = aStar (\p -> directionsO |> map (addPoints p) |> filter inBounds |> filter (`Set.notMember` corrupted) |> map (`pair` 1)) (pointDistanceO (70, 70)) (0, 0) (== (70, 70))
  where
    corrupted = take 1024 s |> map (t2fromList . map readInt . splitOn ',') |> Set.fromList

part2 :: [String] -> String
part2 s = binarySearch (\n -> take n s |> map (t2fromList . map readInt . splitOn ',') |> Set.fromList |> pathBlocked) (0, length s - 1) |> subtract 1
    |> (s !!)

pathBlocked :: Set.Set Point -> Bool
pathBlocked corrupted =
    tryAStar (\p -> directionsO |> map (addPoints p) |> filter inBounds |> filter (`Set.notMember` corrupted) |> map (`pair` 1)) (pointDistanceO (70, 70)) (0, 0) (== (70, 70))
    |> isNothing