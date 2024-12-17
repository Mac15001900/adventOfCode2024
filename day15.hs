{-# LANGUAGE TupleSections #-}

import           Data.Bifunctor ( Bifunctor(bimap) )
import qualified Data.Map       as Map
import           Data.Maybe
import qualified Data.Set       as Set

import           GridUtils

import           MUtils

type Warehouse = (Grid Char, Point) --Objects in the warehouse, robot's position

parseWarehouse :: [String] -> Warehouse
parseWarehouse s = (Map.map (\c -> if c == '@' then '.' else c) grid, getGridPosition grid '@' |> fromJust)
  where
    grid = gridFromList s

parseDirections :: String -> [Dir]
parseDirections [] = []
parseDirections ('^' : xs) = North : parseDirections xs
parseDirections ('v' : xs) = South : parseDirections xs
parseDirections ('<' : xs) = West : parseDirections xs
parseDirections ('>' : xs) = East : parseDirections xs

move :: [Dir] -> Warehouse -> Warehouse
move [] w = w
move (d : ds) (g, r) = case g Map.! r' of
    '#' -> move ds (g, r)
    '.' -> move ds (g, r')
    'O' -> case g Map.! firstNonBoxTile of
        '#' -> move ds (g, r)
        '.' -> move ds (g |> Map.insert r' '.' |> Map.insert firstNonBoxTile 'O', r')
  where
    r' = stepDir d r
    firstNonBoxTile = repeatUntil ((/= 'O') . (g Map.!)) (stepDir d) r'

part1 :: [String] -> Int
part1 s = splitOn "" s |> t2fromList |> bimap parseWarehouse (parseDirections . concat) |> uncurry (flip move) |> fst
    |> Map.foldrWithKey (\(x, y) c s -> s + (if c == 'O' then 100 * y + x else 0)) 0

expand :: String -> String
expand [] = []
expand ('O' : xs) = '[' : ']' : expand xs
expand ('@' : xs) = '@' : '.' : expand xs
expand (x : xs) = x : x : expand xs

move2 :: [Dir] -> Warehouse -> Warehouse
move2 [] w = w
move2 (d : ds) w@(g, r) = case Map.lookup r' g of
    Just '#' -> move2 ds w
    Just '.' -> move2 ds (g, r')
    _ -> if movable d w boxes then move2 ds (newGrid, r') else move2 ds w
  where
    r' = stepDir d r
    boxes = getBoxes d w (pointToBox w r') |> Set.toList
    boxPoints = map t2toList boxes |> concat
    removedBoxes = foldr (`Map.insert` '.') g boxPoints
    newGrid = foldr ((\(b1, b2) g' -> Map.insert b1 '[' g' |> Map.insert b2 ']') . mapBoth (stepDir d)) removedBoxes boxes

type Box = (Point, Point) -- The coordinates of both halves of a box

--Returns all boxes affected by pushing a given box in a given direction
getBoxes :: Dir -> Warehouse -> Box -> Set.Set Box
getBoxes d w@(g, _) b@(b1, b2)
    -- | isHorizontal d = [ b1, b2 ] |> map (moveDir d 2) |> filter (`Map.member` g) |> filter ((`elem` "[") . (g Map.!))
    --     |> map (pointToBox w) |> map (getBoxes d w) |> Set.unions |> Set.insert (b1, b2)

        | isHorizontal d = if moveDir d 2 b1 |> (`Map.lookup` g) |> (== Just '[') then Set.insert (b1, b2) (getBoxes d w (mapBoth (moveDir d 2) b)) else Set.singleton b
        | otherwise = [ b1, b2 ] |> map (stepDir d) |> filter ((`elem` "[]") . (g Map.!)) |> map (pointToBox w) |> map (getBoxes d w) |> Set.unions |> Set.insert (b1, b2)

        -- | isHorizontal d = Set.singleton (b1, b2)
        -- | otherwise = Set.singleton (b1, b2)
pointToBox :: Warehouse -> Point -> Box
pointToBox (g, _) p = case Map.lookup p g of
    Just '[' -> (p, stepDir East p)
    Just ']' -> (stepDir West p, p)

movable :: Dir -> Warehouse -> [Box] -> Bool
-- movable d (g, _) bs = True
movable d (g, _) bs
    | map t2toList bs |> concat |> map (stepDir d) |> any (`Map.notMember` g) = error $ "Checking outside the map " ++ (show (bs, d))
movable d (g, _) bs = map t2toList bs |> concat |> map (stepDir d) |> any ((== '#') . (g Map.!))

part2 :: [String] -> Int
part2 s = splitOn "" s |> t2fromList |> bimap (parseWarehouse . map expand) (parseDirections . concat) |> uncurry (flip move2) |> fst
    |> Map.foldrWithKey (\(x, y) c s -> s + (if c == '[' then 100 * y + x else 0)) 0

{-

partTest :: Int -> [String] -> IO ()
partTest n s = splitOn "" s |> t2fromList |> bimap (parseWarehouse . map expand) (parseDirections . concat) |> uncurry (flip move2) |> fst
    |> Map.foldrWithKey (\(x, y) c s -> s + (if c == '[' then 100 * y + x else 0)) 0
-}
test = [ "########", "#..O.O.#", "##@.O..#", "#...O..#", "#.#.O..#", "#...O..#", "#......#", "########", "", "<^^>>>vv<v>>v<<" ]

testBig =
    [ "##########"
    , "#..O..O.O#"
    , "#......O.#"
    , "#.OO..O.O#"
    , "#..O@..O.#"
    , "#O#..O...#"
    , "#O..O..O.#"
    , "#.OO.O.OO#"
    , "#....O...#"
    , "##########"
    , ""
    , "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
    , "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
    , "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
    , "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
    , "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
    , "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
    , ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
    , "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
    , "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
    , "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    ]