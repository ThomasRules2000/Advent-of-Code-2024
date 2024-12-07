module Days.Day07 where
import           Data.Bifunctor  (bimap)
import           Data.List.Split (splitOn)
import           Data.Maybe      (catMaybes)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple, findNextPow10)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 3749 11387

type Input = [(Int, [Int])]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (bimap read (map read . words) . listToTuple . splitOn ": ") . lines

part1 :: Input -> Output1
part1 = solve False

checkEquation :: Bool -> (Int, [Int]) -> Bool
checkEquation _ (_, []) = False
checkEquation allowConcat (target, first:rest) = go first rest
    where
        go :: Int -> [Int] -> Bool
        go acc [] = target == acc
        go acc (x:xs) = or $ catMaybes [tryAdd, tryMul, tryConcat]
            where
                addResult = acc + x
                mulResult = acc * x
                concatResult = acc * findNextPow10 x + x
                tryAdd = if addResult <= target then Just $ go addResult xs else Nothing
                tryMul = if mulResult <= target then Just $ go mulResult xs else Nothing
                tryConcat = if allowConcat && concatResult <= target then Just $ go concatResult xs else Nothing

solve :: Bool -> [(Int, [Int])] -> Int
solve allowConcat = sum . map fst . filter (checkEquation allowConcat)

part2 :: Input -> Output2
part2 = solve True
