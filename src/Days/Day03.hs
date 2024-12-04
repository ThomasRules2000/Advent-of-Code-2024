{-# LANGUAGE OverloadedStrings #-}

module Days.Day03 where
import           Control.Applicative              (many)
import           Control.Applicative.Combinators  (choice)
import           Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char,
                                                   decimal, string)
import           Data.Composition                 ((.:))
import           Data.Foldable                    (foldl')
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

data Instruction = Mul Int | ChangeEna Bool

parser :: Parser Input
parser = catMaybes <$> many (choice [
    (Just .: Mul .: (*))   <$ string "mul(" <*> decimal <* char ',' <*> decimal <* char ')',
    Just (ChangeEna True)  <$ string "do()",
    Just (ChangeEna False) <$ string "don't()",
    Nothing                <$ anyChar])

part1 :: Input -> Output1
part1 = foldl' (\acc instr -> case instr of {Mul x -> x + acc; _ -> acc}) 0

part2 :: Input -> Output2
part2 = snd . foldl' go (True, 0)
    where
        go :: (Bool, Int) -> Instruction -> (Bool, Int)
        go (_, acc) (ChangeEna e) = (e, acc)
        go (e, acc) (Mul x)       = (e, if e then x + acc else acc)
