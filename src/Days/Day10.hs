module Days.Day10 where
import           Data.Char       (digitToInt)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import qualified Util.Map        as Map

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 36 81

type Input = Map (Int, Int) Int

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.fromGrid . map (map digitToInt) . lines

part1 :: Input -> Output1
part1 = solve Map.size

part2 :: Input -> Output2
part2 = solve sum

getAround :: (Int, Int) -> Set (Int, Int)
getAround (x, y) = Set.fromAscList [(x-1, y), (x, y-1), (x, y+1), (x+1, y)]

solve :: (Map (Int, Int) Int -> Int) -> Map (Int, Int) Int -> Int
solve f m = sum $ map (f . getTrails m) $ Map.keys $ Map.filter (== 0) m

getTrails :: Map (Int, Int) Int -> (Int, Int) -> Map (Int, Int) Int
getTrails m = go 0 . (`Map.singleton` 1)
    where
        go :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int
        go 9 = id
        go n = go (n+1)
             . Map.unionsWith (+)
             . Map.mapWithKey (\pos score -> Map.fromSet (const score)
                                           $ Set.filter ((Just (n+1) ==) . flip Map.lookup m)
                                           $ getAround pos)
