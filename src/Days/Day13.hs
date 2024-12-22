module Days.Day13 where
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple, listToTuple3)
import Data.Tuple.Extra (both)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 480 0

type Input = [((Int, Int), (Int, Int), (Int, Int))]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (listToTuple3 . map (listToTuple
                                 . map (read . drop 2)
                                 . splitOn ", "
                                 . snd
                                 . listToTuple
                                 . splitOn ": ")
                           . lines)
       . splitOn "\n\n"

part1 :: Input -> Output1
part1 = sum . map solveOne

getPossibilities :: Int -> Int -> Int -> Set (Int, Int)
getPossibilities a b res = Set.fromAscList $ mapMaybe findOther [0..end]
    where
        end = res `div` a

        findOther :: Int -> Maybe (Int, Int)
        findOther numA
            | remainder /= 0 = Nothing
            | otherwise = Just (numA, numB)
            where
                target = res - (a * numA)
                (numB, remainder) = target `divMod` b

solveOne :: ((Int, Int), (Int, Int), (Int, Int)) -> Int
solveOne ((xA, yA), (xB, yB), (resA, resB))
    | Set.null poss = 0
    | otherwise = Set.findMin $ Set.map score poss
    where
        poss = getPossibilities xA xB resA `Set.intersection` getPossibilities yA yB resB

        score :: (Int, Int) -> Int
        score (numA, numB) = (3 * numA) + numB

part2 :: Input -> Output2
part2 = undefined -- sum . map (solveOne . fmap (both (+10000000000000)))
