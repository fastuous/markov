module Main where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as A
import Data.Attoparsec.Combinator (lookAhead)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type Chain  = M.Map Prefix Suffix
type Prefix = T.Text
type Suffix = M.Map T.Text Int

test :: T.Text
test = T.pack "The quick brown fox jumps over the lazy dog."

test' :: Int -> T.Text
test' n = nonword `T.append` test `T.append` nonword where
  nonword = T.pack $ replicate n '\NUL'

parsePair :: Int -> Parser (Prefix, Suffix)
parsePair ord = lookAhead pair <* word where
  pair = (,) <$> foldr T.append T.empty <$> replicateM ord word
             <*> (M.singleton <$> word <*> pure 1)

word :: Parser T.Text
word = T.singleton <$> char '\NUL' <|>
       T.append <$> A.takeWhile1 isWordChar <*> A.takeWhile isSpace where
         isWordChar = (&&) <$> not . isSpace <*> (/= '\NUL')

buildChain :: Int -> T.Text -> Chain
buildChain ord input =
  M.fromListWith (M.unionWith (+)) pairList where
    pairList = either (const []) id $ parseOnly (many (parsePair ord)) input'
    input' = nonword `T.append` input `T.append` nonword
    nonword = T.pack $ replicate ord '\NUL'

main :: IO ()
main = do
  putStrLn "hello world"
