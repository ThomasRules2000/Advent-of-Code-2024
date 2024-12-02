module Days.Day02 where
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 2 4

type Input = [[Int]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map read . words) . lines

part1 :: Input -> Output1
part1 = length . filter checkSafe

checkSafe :: [Int] -> Bool
checkSafe xs = checkCond checkDiff xs && (checkCond (<) xs || checkCond (>) xs)
    where
        checkCond :: (Int -> Int -> Bool) -> [Int] -> Bool
        checkCond p xs = and $ zipWith p xs $ tail xs

        checkDiff :: Int -> Int -> Bool
        checkDiff x y = diff >= 1 && diff <= 3
            where diff = abs $ x - y


part2 :: Input -> Output2
part2 = length . filter checkSafe'

checkSafe' :: [Int] -> Bool
checkSafe' xs = any (checkSafe . uncurry (++) . fmap tail . (`splitAt` xs)) [0..length xs - 1]