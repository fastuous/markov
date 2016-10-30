module Main where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Attoparsec.Combinator (lookAhead)
import           Data.Attoparsec.Text       as A
import           Data.Char                  (isSpace)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.Environment         (getArgs)
import           System.Random

type Chain  = M.Map Prefix Suffix
type Prefix = T.Text
type Suffix = M.Map T.Text Int

type MarkovGen = ReaderT (Token, Chain) (StateT (StdGen, Prefix) IO) ()

data Token = Word | Char deriving Eq

parsePair :: Parser T.Text -> Int -> Parser (Prefix, Suffix)
parsePair p ord = lookAhead pair <* p where
  pair = (,) <$> foldr T.append T.empty <$> replicateM ord p
             <*> (M.singleton <$> p <*> pure 1)

word :: Parser T.Text
word = T.singleton <$> char '\NUL' <|>
       T.append <$> A.takeWhile1 isWordChar <*> A.takeWhile isSpace where
         isWordChar = (&&) <$> not . isSpace <*> (/= '\NUL')

buildChain :: Token -> Int -> T.Text -> Chain
buildChain tok ord input =
  M.fromListWith (M.unionWith (+)) pairList where
    pairList = either (const []) id $ parseOnly (many (parsePair p ord)) input'
    p = if tok == Word then word else T.singleton <$> anyChar
    input' = nonword `T.append` input `T.append` nonword
    nonword = T.pack $ replicate ord '\NUL'

shiftPrefix :: Token -> Prefix -> T.Text -> Prefix
shiftPrefix tok prefix suffix = prefix' `T.append` suffix where
  prefix' = if tok == Char || T.head prefix == '\NUL'
            then T.tail prefix
            else T.dropWhile isSpace (T.dropWhile isWordChar prefix)
  isWordChar = (&&) <$> not . isSpace <*> (/= '\NUL')

markovGen :: MarkovGen
markovGen = do
  (g, prefix) <- get
  (token, chain) <- ask
  let suffMap    = chain M.! prefix
      totalFreq  = M.foldr (+) 0 suffMap
      suffMap'   = snd $ M.mapAccum (\a b -> (a + b, a + b)) 0 suffMap
      (rand, g') = randomR (1, totalFreq) g
      suffix     = fst . M.findMin . M.filter (>= rand) $ suffMap'
      prefix'    = shiftPrefix token prefix suffix
  liftIO $ T.putStr suffix
  put (g', prefix')

evalMarkov :: MarkovGen -> (Token, Chain) -> (StdGen, Prefix) -> IO ()
evalMarkov m r = evalStateT (runReaderT m r)

generate :: Token -> Int -> Int -> Chain -> IO ()
generate tok n ord chain = getStdGen >>= \g ->
  evalMarkov (replicateM_ n markovGen) (tok, chain) (g, start) where
    start = T.pack $ replicate ord '\NUL'

main :: IO ()
main = do
  args <- getArgs
  let ord   = read (head args) :: Int
      n     = read (args!!1) :: Int
      token = if args!!2 == "word" then Word else Char
      files = mapM T.readFile (drop 3 args)
  inputs <- files
  let chains = map (buildChain token ord) inputs
      chain  = foldr (M.unionWith (M.unionWith (+))) M.empty chains
  generate token n ord chain >> putChar '\n'
