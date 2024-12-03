module Days.Day01 where
import           Data.Bifunctor   (bimap)
import           Data.Composition ((.:))
import           Data.List        (group, sort, transpose)
import qualified Data.Map.Strict  as Map
import           Data.Tuple.Extra (both, dupe)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import           Util.Util        (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 11 31

type Input = ([Int], [Int])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = listToTuple
         . map sort
         . transpose
         . map (map read . words)
         . lines

part1 :: Input -> Output1
part1 = sum . uncurry (zipWith (abs .: subtract))

part2 :: Input -> Output2
part2 = sum
        . uncurry (Map.intersectionWithKey ((*) .: (*)))
        . both (Map.fromDistinctAscList
                . map (bimap head length . dupe)
                . group)
