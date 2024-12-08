module Days.Day08 where
import           Data.Bifunctor   (bimap)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (dupe, swap)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import qualified Util.Map         as Map

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 14 34

type Input = (Map Char [(Int, Int)], (Int, Int))

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (invert . Map.filter (/= '.')) (fst . Map.findMax) . dupe . Map.fromGrid . lines

invert :: Map (Int, Int) Char -> Map Char [(Int, Int)]
invert = Map.fromListWith (<>) . map (fmap (: []) . swap) . Map.toList

part1 :: Input -> Output1
part1 (m, bounds) = Set.size $ Set.unions $ Map.map (calcAntinodes bounds) m

calcAntinodes :: (Int, Int) -> [(Int, Int)] -> Set (Int, Int)
calcAntinodes (maxX, maxY) nodes = Set.unions [antinodes pos1 pos2 | pos1 <- nodes, pos2 <- nodes]
    where
        antinodes :: (Int, Int) -> (Int, Int) -> Set (Int, Int)
        antinodes pos1@(x1, y1) pos2@(x2, y2)
            | pos1 == pos2 = mempty
            | otherwise = Set.fromList $ filter (\(x, y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY)
                                         [(x1 + diffX, y1 + diffY), (x2 - diffX, y2 - diffY)]
            where
                diffX = x1 - x2
                diffY = y1 - y2

part2 :: Input -> Output2
part2 (m, bounds) = Set.size $ Set.unions $ Map.map (calcAntinodes' bounds) m

calcAntinodes' :: (Int, Int) -> [(Int, Int)] -> Set (Int, Int)
calcAntinodes' (maxX, maxY) nodes = Set.unions [antinodes pos1 pos2 | pos1 <- nodes, pos2 <- nodes]
    where
        antinodes :: (Int, Int) -> (Int, Int) -> Set (Int, Int)
        antinodes pos1@(x1, y1) pos2@(x2, y2)
            | pos1 == pos2 = mempty
            | otherwise = Set.fromList [(x, y) | mul <- [-mulMax..mulMax],
                                                 let x = x1 + (mul * diffX),
                                                 let y = y1 + (mul * diffY),
                                                 x >= 0, y >= 0, x <= maxX, y <= maxY]
            where
                rawDiffX = x1 - x2
                rawDiffY = y1 - y2

                diffX = rawDiffX `div` gcd rawDiffX rawDiffY
                diffY = rawDiffY `div` gcd rawDiffX rawDiffY

                mulMax = max maxX maxY
