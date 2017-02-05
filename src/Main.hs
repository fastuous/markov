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

-- The suffix map maps text values to their corresponding frequency of occurence
type Suffix = M.Map T.Text Int

-- main monad stack that exposes the token and lookup table in an anonymous type in
-- the reader monad, as well as the current table key (prefix) and a random number generator
-- in the state monad
type MarkovGen = ReaderT (Token, Chain) (StateT (StdGen, Prefix) IO) ()

data Token = Word | Char deriving Eq

-- advances the parser state by one text token at a time, while processing ord number
-- of tokens, using a lookahead parser. Returns the parsed prefix and suffix pair
parsePair :: Parser T.Text -> Int -> Parser (Prefix, Suffix)
parsePair p ord = lookAhead pair <* p where
  pair = (,) <$> (foldr T.append T.empty <$> count ord p)
             <*> (M.singleton <$> p <*> pure 1)

-- Parses a "word", treating a \NUL character as a word; takes advantage of the
-- documented efficiency of Attoparsec's takeWhile parser
word :: Parser T.Text
word =  T.singleton <$> char '\NUL'
    <|> T.append <$> A.takeWhile1 isWordChar <*> A.takeWhile isSpace where
          isWordChar = (&&) <$> not . isSpace <*> (/= '\NUL') -- applicative of functions,
                                                              -- sacrificing readability for a little fun


buildChain :: Token -> Int -> T.Text -> Chain
buildChain tok ord input =
  M.fromListWith (M.unionWith (+)) pairList where
    pairList = either (const []) id $ parseOnly (many (parsePair p ord)) input'
    p = if tok == Word then word else T.singleton <$> anyChar -- choose the parser based on the chosen
                                                              -- token type

    input' = nonword `T.append` input `T.append` nonword -- append and prepend "NONWORD" to the input,
                                                         -- as suggested in the spec on page 4

    nonword = T.pack $ replicate ord '\NUL'

-- Helper function to get the next prefix given the chosen suffix. Readability
-- might be improved by unpacking and repacking the Text object into a string.
shiftPrefix :: Token -> Prefix -> T.Text -> Prefix
shiftPrefix tok prefix suffix = prefix' `T.append` suffix where
  prefix' = if tok == Char || T.head prefix == '\NUL'
            then T.tail prefix
            else T.dropWhile isSpace (T.dropWhile isWordChar prefix)
  isWordChar = (&&) <$> not . isSpace <*> (/= '\NUL')

markovGen :: MarkovGen
markovGen = do
  (g, prefix)    <- get
  (token, chain) <- ask
  let suffMap           = chain M.! prefix
      (total, suffMap') = M.mapAccum (\a b -> (a + b, a + b)) 0 suffMap -- reshuffle the suffix map
                                                                        -- to have suffix frequencies correspond
                                                                        -- to an ascending sum of probabilities for
                                                                        -- the random number generator
      (rand, g')        = randomR (1, total) g
      suffix            = fst . M.findMin . M.filter (>= rand) $ suffMap' -- use the random number to pick a suffix
                                                                          -- with respect to its probability weight
      prefix'           = shiftPrefix token prefix suffix
  liftIO $ T.putStr suffix
  put (g', prefix')

-- sets up the repeated main monadic sequence above
generate :: Token -> Int -> Int -> Chain -> IO ()
generate tok n ord chain = getStdGen >>= \g ->
  evalMarkov (replicateM_ n markovGen) (tok, chain) (g, start) where
    start = T.pack $ replicate ord '\NUL' -- the initial prefix
    evalMarkov m r = evalStateT (runReaderT m r)

-- Reads the command line arguments as detailed by the spec on page 3, assumes valid input.
main :: IO ()
main = do
  args <- getArgs
  let ord   = read (head args) :: Int
      n     = read (args!!1)   :: Int
      token = if args!!2 == "word" then Word else Char
      files = mapM T.readFile (drop 3 args)
  inputs <- files
  let chains = map (buildChain token ord) inputs -- makes a markov table for each input source
      chain  = foldr (M.unionWith (M.unionWith (+))) M.empty chains -- utilizes map functions to merge the markov
                                                                    -- tables of each source into one single markov table
  generate token n ord chain >> putChar '\n'
