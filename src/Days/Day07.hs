module Days.Day07 where
import           Data.Bifunctor  (bimap)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
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
checkEquation allowConcat (target, first:rest) = elem first $ go rest
    where
        go :: [Int] -> [Int]
        go [] = [target]
        go (x:xs) = concatMap funcs $ go xs
            where funcs acc = mapMaybe ($ acc) [checkAdd x, checkMul x, checkConcat x]

        checkAdd :: Int -> Int -> Maybe Int
        checkAdd x acc = if acc > x then Just $ acc - x else Nothing

        checkMul :: Int -> Int -> Maybe Int
        checkMul x acc = if (acc `mod` x) == 0 then Just $ acc `div` x else Nothing

        checkConcat :: Int -> Int -> Maybe Int
        checkConcat x acc = if allowConcat && (acc `mod` pow10) == x then Just $ acc `div` pow10 else Nothing
            where pow10 = findNextPow10 x

solve :: Bool -> [(Int, [Int])] -> Int
solve allowConcat = sum . map fst . filter (checkEquation allowConcat)

part2 :: Input -> Output2
part2 = solve True
