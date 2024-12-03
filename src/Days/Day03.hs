{-# LANGUAGE OverloadedStrings #-}

module Days.Day03 where
import           Control.Applicative              (many)
import           Control.Applicative.Combinators  (choice)
import           Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char,
                                                   decimal, string)
import           Data.Maybe                       (catMaybes)
import qualified Program.RunDay                   as R (runDay)
import qualified Program.TestDay                  as T (testDay)
import           System.Clock                     (TimeSpec)
import           Test.Hspec                       (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 161 48

type Input = [Instruction]

type Output1 = Int
type Output2 = Int

data Instruction = Mul (Int, Int) | Do | Don't

parser :: Parser Input
parser = catMaybes <$> many (choice [
    Just       <$> parseMul,
    Just Do    <$  string "do()",
    Just Don't <$  string "don't()",
    Nothing    <$  anyChar])

parseMul :: Parser Instruction
parseMul = do
    string "mul("
    x <- decimal
    char ','
    y <- decimal
    char ')'
    pure $ Mul (x, y)

part1 :: Input -> Output1
part1 = sum . map (uncurry (*)) . getMuls

getMuls :: [Instruction] -> [(Int, Int)]
getMuls []           = []
getMuls ((Mul x):xs) = x:getMuls xs
getMuls (_:xs)       = getMuls xs

part2 :: Input -> Output2
part2 = sum . map (uncurry (*)) . getMuls'

getMuls' :: [Instruction] -> [(Int, Int)]
getMuls' = go True
    where
        go :: Bool -> [Instruction] -> [(Int, Int)]
        go _     []           = []
        go _     (Do:xs)      = go True xs
        go _     (Don't:xs)   = go False xs
        go True  ((Mul x):xs) = x:go True xs
        go False ((Mul _):xs) = go False xs
