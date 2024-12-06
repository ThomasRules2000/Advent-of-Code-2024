module Days.Day06 where
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Cycle      (Cycle, next)
import qualified Util.Map        as Map

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 41 6

type Input = ((Int, Int), Set (Int, Int), (Int, Int))

type Output1 = Int
type Output2 = Int

data Facing = North | East | South | West
    deriving (Eq, Ord, Show, Enum, Bounded, Cycle)

parser :: String -> Input
parser input = (guard, walls, bounds)
    where
        m = Map.fromGrid $ lines input
        guard = head $ Map.keys $ Map.filter (=='^') m
        walls = Map.keysSet $ Map.filter (=='#') m
        bounds = fst $ Map.findMax m

part1 :: Input -> Output1
part1 (startPos, walls, (maxX, maxY)) = Set.size $ go startPos North
    where
        go :: (Int, Int) -> Facing -> Set (Int, Int)
        go pos@(x, y) face
            | (x < 0) || (y < 0) || (x > maxX) || (y > maxY) = mempty
            | otherwise = Set.insert pos $ go newPos newFace
            where (newPos, newFace) = move walls pos face

nextPos :: (Int, Int) -> Facing -> (Int, Int)
nextPos (x, y) North = (x - 1, y)
nextPos (x, y) South = (x + 1, y)
nextPos (x, y) East  = (x, y + 1)
nextPos (x, y) West  = (x, y - 1)

move :: Set (Int, Int) -> (Int, Int) -> Facing -> ((Int, Int), Facing)
move walls pos face
    | newPos `Set.member` walls = (pos, next face)
    | otherwise                 = (newPos, face)
    where newPos = nextPos pos face

part2 :: Input -> Output2
part2 (startPos, walls, (maxX, maxY))= Set.size $ go startPos North mempty
    where
        go :: (Int, Int) -> Facing -> Set ((Int, Int), Facing) -> Set (Int, Int)
        go pos face visited
            | (newX < 0) || (newY < 0) || (newX > maxX) || (newY > maxY) = mempty
            | any (`Set.member` newVisited) [(newPos, North), (newPos, South), (newPos, East), (newPos, West)] = rest
            | checkLoop (Set.insert newPos walls) pos face visited = Set.insert newPos rest
            | otherwise = rest
            where
                (newPos@(newX, newY), newFace) = move walls pos face
                newVisited = Set.insert (pos, face) visited
                rest = go newPos newFace newVisited


        checkLoop :: Set (Int, Int) -> (Int, Int) -> Facing -> Set ((Int, Int), Facing) -> Bool
        checkLoop newWalls pos@(x, y) face visited
            | (x < 0) || (y < 0) || (x > maxX) || (y > maxY) = False
            | (pos, face) `Set.member` visited = True
            | otherwise = checkLoop newWalls newPos newFace $ Set.insert (pos, face) visited
                where (newPos, newFace) = move newWalls pos face
