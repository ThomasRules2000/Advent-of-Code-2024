module Days.Day11 where
import           Control.Monad.Trans.State      (modify, evalState)
import           Control.Monad.Trans.State.Lazy (State, gets)
import           Data.Functor                   (($>))
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import qualified Program.RunDay                 as R (runDay)
import qualified Program.TestDay                as T (testDay)
import           System.Clock                   (TimeSpec)
import           Test.Hspec                     (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 55312 0

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map read . words

part1 :: Input -> Output1
part1 = solve 25

evolve :: Int -> [Int]
evolve 0 = [1]
evolve n
    | even numDigits = [n1, n2]
    | otherwise = [n * 2024]
    where
        numDigits = ceiling $ logBase 10 $ fromIntegral (n + 1)
        (n1, n2) = n `divMod` (10 ^ (numDigits `div` 2))

evolve' :: Int -> State (Map Int [Int]) [Int]
evolve' 0 = pure [1]
evolve' n = gets (Map.lookup n) >>= \case
        Just res -> pure res
        Nothing -> modify (Map.insert n res) $> res
            where
                res = if even numDigits then [n1, n2] else [n * 2024]
                numDigits = ceiling $ logBase 10 $ fromIntegral (n + 1)
                (n1, n2) = n `divMod` (10 ^ (numDigits `div` 2))

solve :: Int -> [Int] -> Int
solve n = length . (`evalState` mempty) . (!! n) . iterate (>>= (fmap concat . mapM evolve')) . pure

part2 :: Input -> Output2
part2 = undefined -- solve 75

type Cache = Map Int (Map Int [Int])

-- evolve'' :: Int -> Map (Int, Int) Int -> State Cache (Map Int Int)
-- evolve'' end m
--     | nextTime == end = m
--     | otherwise = gets (Map.lookup nextNum m) >>= \case
--         Nothing -> do
--             let new = evolve nextNum
--     where ((nextTime, nextNum), nextCount) = Map.findMin m

