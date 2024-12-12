module Days.Day12 where
import           Data.Containers.ListUtils (nubOrd)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Program.RunDay            as R (runDay)
import qualified Program.TestDay           as T (testDay)
import           System.Clock              (TimeSpec)
import           Test.Hspec                (Spec)
import qualified Util.Map                  as Map
import Data.Bifunctor (Bifunctor(second), first)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 1930 1206

type Input = Map (Int, Int) Char

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.fromGrid . lines

part1 :: Input -> Output1
part1 = sum . map (\r -> area r * perimeter r) . getRegions

getRegions :: Map (Int, Int) Char -> [Set (Int, Int)]
getRegions m = concatMap (floodRegions . Map.keysSet . (`Map.filter` m) . (==)) $ nubOrd $ Map.elems m

floodRegions :: Set (Int, Int) -> [Set (Int, Int)]
floodRegions points
    | Set.null points = []
    | otherwise = newRegion : floodRegions (points Set.\\ newRegion)
    where
        newRegion = floodRegion (Set.singleton $ Set.findMin points) mempty

        floodRegion :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
        floodRegion frontier closed
            | Set.null frontier = mempty
            | otherwise = Set.union frontier $ floodRegion newFrontier (Set.union frontier closed)
            where newFrontier = Set.intersection points $ (Set.\\ closed) $ Set.unions $ Set.map getAround frontier

area :: Set (Int, Int) -> Int
area = Set.size

perimeter :: Set (Int, Int) -> Int
perimeter points = sum $ map (Set.size . (Set.\\ points) . getAround) $ Set.toList points

getAround :: (Int, Int) -> Set (Int, Int)
getAround (x, y) = Set.fromAscList [(x-1, y), (x, y-1), (x, y+1), (x+1, y)]

part2 :: Input -> Output2
part2 = sum . map (\r -> area r * sides r) . getRegions

data Facing = North | East | South | West
    deriving (Eq, Ord, Show)

getAroundFacing :: (Int, Int) -> Set ((Int, Int), Facing)
getAroundFacing (x, y) = Set.fromAscList [((x-1, y), North), ((x, y-1), East), ((x, y+1), West), ((x+1, y), South)]

sides :: Set (Int, Int) -> Int
sides points = Set.size $ combineSides $ Set.unions $ map (Set.filter (\(pos, _) -> Set.notMember pos points) . getAroundFacing) $ Set.toList points

combineSides :: Set ((Int, Int), Facing) -> Set (Set ((Int, Int), Facing))
combineSides perimPoints = Set.map side perimPoints
    where
        walk :: ((Int, Int) -> (Int, Int)) -> ((Int, Int), Facing) -> Set ((Int, Int), Facing)
        walk f posFace@(pos, face)
            | posFace `Set.member` perimPoints = Set.insert posFace $ walk f (f pos, face)
            | otherwise = mempty

        side :: ((Int, Int), Facing) -> Set ((Int, Int), Facing)
        side posFace@(_, face) = case face of
            North -> ns_side
            South -> ns_side
            East -> ew_side
            West -> ew_side
            where
                ns_side = Set.union (walk (second (subtract 1)) posFace) (walk (second (+1)) posFace)
                ew_side = Set.union (walk (first (subtract 1)) posFace) (walk (first (+1)) posFace)
