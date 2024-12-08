module Days.Day04 where
import           Data.Matrix           (Matrix)
import qualified Data.Matrix           as Matrix
import qualified Program.RunDay        as R (runDay)
import qualified Program.TestDay       as T (testDay)
import           System.Clock          (TimeSpec)
import           Test.Hspec            (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 18 9

type Input = Matrix Char

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Matrix.fromLists . lines

part1 :: Input -> Output1
part1 m = sum [countXmas m (x, y) | x <- [1..Matrix.nrows m], y <- [1..Matrix.ncols m]]

countXmas :: Matrix Char -> (Int, Int) -> Int
countXmas m (x, y) = length $ filter checkXmas [row, col, fwDiag, bwDiag]
    where
        row = [(x', y) | x' <- [x..x+3]]
        col = [(x, y') | y' <- [y..y+3]]
        fwDiag = [(x-n, y+n) | n <- [0..3]]
        bwDiag = [(x+n, y+n) | n <- [0..3]]

        checkXmas :: [(Int, Int)] -> Bool
        checkXmas idxs = word == Just "XMAS" || word == Just "SAMX"
            where word = traverse (flip (uncurry Matrix.safeGet) m) idxs

part2 :: Input -> Output2
part2 m = length $ filter (isCrossMas m) [(x, y) | x <- [2..Matrix.nrows m - 1], y <- [2..Matrix.ncols m - 1]]

isCrossMas :: Matrix Char -> (Int, Int) -> Bool
isCrossMas m (x, y) = cValid && fwValid && bwValid
    where
        tl = m Matrix.! (x-1, y-1)
        tr = m Matrix.! (x+1, y-1)
        bl = m Matrix.! (x-1, y+1)
        br = m Matrix.! (x+1, y+1)
        c = m Matrix.! (x, y)

        cValid = c == 'A'
        fwValid = (tl == 'M' && br == 'S') || (tl == 'S' && br == 'M')
        bwValid = (tr == 'M' && bl == 'S') || (tr == 'S' && bl == 'M')
