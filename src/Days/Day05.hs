module Days.Day05 where
import           Data.Bifunctor  (bimap)
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 143 123

type Input = (Set (Int, Int), [[Int]])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (Set.fromList . map (listToTuple . map read . splitOn "|")) (map (map read . splitOn ","))
       . listToTuple
       . map lines
       . splitOn "\n\n"

part1 :: Input -> Output1
part1 (rules, reports) = sum $ mapMaybe (getValids rules) reports

getValids :: Set (Int, Int) -> [Int] -> Maybe Int
getValids rules report = if checkValid rules report then Just $ report !! (length report `div` 2) else Nothing

checkValid :: Set (Int, Int) -> [Int] -> Bool
checkValid rules = Set.disjoint rules . Set.fromList . getPairs

getPairs :: [Int] -> [(Int, Int)]
getPairs []     = []
getPairs (x:xs) = map (,x) xs ++ getPairs xs

part2 :: Input -> Output2
part2 (rules, reports) = sum $ mapMaybe (getInvalids rules) reports

getInvalids :: Set (Int, Int) -> [Int] -> Maybe Int
getInvalids rules report = if checkValid rules report then Nothing else Just $ sorted !! (length report `div` 2)
    where sorted = reorder rules report

reorder :: Set (Int, Int) -> [Int] -> [Int]
reorder rules = sortBy getOrder
    where
        getOrder :: Int -> Int -> Ordering
        getOrder x y
            | (x, y) `Set.member` rules = LT
            | (y, x) `Set.member` rules = GT
            | otherwise = EQ
