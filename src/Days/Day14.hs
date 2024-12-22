module Days.Day14 where
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 12 0

type Input = [((Int, Int), (Int, Int))]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (listToTuple . map (listToTuple . map read . splitOn "," . drop 2) . words). lines

part1 :: Input -> Output1
part1 = product . Map.fromListWith (+) . mapMaybe (fmap (, 1) . toQuadrant . move100)

maxX :: Int
maxX = 101

maxY :: Int
maxY = 103

move100 :: ((Int, Int), (Int, Int)) -> (Int, Int)
move100 ((px, py), (vx, vy)) = ((px + 100 * vx) `mod` maxX, (py + 100 * vy) `mod` maxY)

toQuadrant :: (Int, Int) -> Maybe (Bool, Bool)
toQuadrant (x, y)
    | x == midX || y == midY = Nothing
    | otherwise = Just (x < midX, y < midY)
    where
        midX = maxX `div` 2
        midY = maxY `div` 2

part2 :: Input -> Output2
part2 = undefined
