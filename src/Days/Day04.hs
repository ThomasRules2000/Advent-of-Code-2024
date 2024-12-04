module Days.Day04 where
import           Data.List             (transpose)
import           Data.List.Split       (splitOn)
import           Data.Matrix           (Matrix)
import qualified Data.Matrix           as Matrix
import           Data.Universe.Helpers (diagonals)
import qualified Program.RunDay        as R (runDay)
import qualified Program.TestDay       as T (testDay)
import           System.Clock          (TimeSpec)
import           Test.Hspec            (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 18 9

type Input = [String]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = lines

part1 :: Input -> Output1
part1 xs = countRows xs + countCols xs + countForwardDiags xs + countBackwardDiags xs

countXmas :: String -> Int
countXmas = subtract 1 . length . splitOn "XMAS"

countRows :: [String] -> Int
countRows xs = sum (map countXmas xs) + sum (map (countXmas . reverse) xs)

countCols :: [String] -> Int
countCols = countRows . transpose

countForwardDiags :: [String] -> Int
countForwardDiags = countRows . diagonals

countBackwardDiags :: [String] -> Int
countBackwardDiags = countForwardDiags . map reverse

part2 :: Input -> Output2
part2 ss = length $ filter (isCrossMas m) [(x, y) | x <- [1..Matrix.nrows m - 2], y <- [1..Matrix.ncols m - 2]]
    where m = Matrix.fromLists ss

isCrossMas :: Matrix Char -> (Int, Int) -> Bool
isCrossMas m (x, y) = cValid && fwValid && bwValid
    where
        tl = m Matrix.! (x, y)
        tr = m Matrix.! (x+2, y)
        bl = m Matrix.! (x, y+2)
        br = m Matrix.! (x+2, y+2)
        c = m Matrix.! (x+1, y+1)

        cValid = c == 'A'
        fwValid = (tl == 'M' && br == 'S') || (tl == 'S' && br == 'M')
        bwValid = (tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M')
