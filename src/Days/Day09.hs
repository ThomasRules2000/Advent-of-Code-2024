module Days.Day09 where
import           Data.Char       (digitToInt)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 1928 2858

type Input = Map Int (Int, Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = getBlocks . map digitToInt

getBlocks :: [Int] -> Map Int (Int, Int)
getBlocks = go 0 0
    where
        go :: Int -> Int -> [Int] -> Map Int (Int, Int)
        go _ _ [] = mempty
        go blockNo pos [x] = Map.singleton pos (x, blockNo)
        go blockNo pos (file:empty:rest) = Map.insert pos (file, blockNo) $ go (blockNo + 1) (pos + file + empty) rest

part1 :: Input -> Output1
part1 = checksum . moveBlocks 0

moveBlocks :: Int -> Map Int (Int, Int) -> Map Int (Int, Int)
moveBlocks pos m = case Map.lookupLE pos m of
        Nothing -> error $ "Search went wrong at pos=" <> show pos
        Just (curPos, (curSize, _))
            | pos < insertionPos -> moveBlocks (curPos + curSize) m
            | otherwise -> case Map.lookupGT pos m of
                Nothing -> m
                Just (nextPos, (nextSize, _))
                    | newEnd <= nextPos -> moveBlocks newEnd
                                            $ Map.insert insertionPos (lastSize, lastNum) newMap
                    | otherwise -> moveBlocks (nextPos + nextSize)
                                    $ Map.insert insertionPos (insertionSize, lastNum)
                                    $ Map.insert lastPos (lastSize - insertionSize, lastNum) newMap
                    where insertionSize = nextPos - insertionPos
            where
                insertionPos = curPos + curSize
                newEnd = insertionPos + lastSize
                ((lastPos, (lastSize, lastNum)), newMap) = Map.deleteFindMax m

checksum :: Map Int (Int, Int) -> Int
checksum = Map.foldrWithKey calcChecksum 0
    where
        calcChecksum :: Int -> (Int, Int) -> Int -> Int
        calcChecksum pos (size, val) acc = acc + val * (((pos + size) * (pos + size - 1) `div` 2) - ((pos * (pos - 1)) `div` 2))


part2 :: Input -> Output2
part2 = checksum . moveBlocks'

moveBlocks' :: Map Int (Int, Int) -> Map Int (Int, Int)
moveBlocks' initMap = go (Map.toDescList initMap) initMap
    where
        go :: [(Int, (Int, Int))] -> Map Int (Int, Int) -> Map Int (Int, Int)
        go [] m = m
        go (block@(curPos, curSizeVal):rest) m = case findFreeSpace block $ Map.toAscList m of
            Nothing -> go rest m
            Just newPos -> go rest $ Map.insert newPos curSizeVal $ Map.delete curPos m

        findFreeSpace :: (Int, (Int, Int)) -> [(Int, (Int, Int))] -> Maybe Int
        findFreeSpace (_, (_, _)) [] = Nothing
        findFreeSpace (_, (_, _)) [_] = Nothing
        findFreeSpace block@(pos, (size, _)) ((lPos, (lSize, _)):rest@((rPos, _):_))
            | lEnd >= pos = Nothing
            | rPos - lEnd >= size = Just lEnd
            | otherwise = findFreeSpace block rest
            where lEnd = lPos + lSize