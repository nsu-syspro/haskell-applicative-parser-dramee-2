{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
  | EmptyParser     -- ^ Parser that always fails without consuming input

 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse parser str = runParser parser (Position 0 str)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe parser str = case parse parser str of
  Parsed a _ -> Just a
  Failed _ -> Nothing

instance Functor Parser where
  fmap f parserA = Parser g where
    g input = case runParser parserA input of
      Parsed x rest -> Parsed (f x) rest
      Failed errs -> Failed errs

instance Applicative Parser where
  pure x = Parser $ \input -> Parsed x input
  (<*>) parserF parserA = Parser g where
    g input = case runParser parserF input of
      Parsed f rest -> case runParser parserA rest of
        Parsed x rest' -> Parsed (f x) rest'
        Failed errs -> Failed errs
      Failed errs -> Failed errs

instance Alternative Parser where
  empty = Parser $ \_ -> Failed [Position 0 EmptyParser]
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) parserA parserB = Parser g where
    g input = case runParser parserA input of
      Parsed x rest -> Parsed x rest
      Failed errsA -> case runParser parserB input of
        Parsed x rest -> Parsed x rest
        Failed errsB -> Failed (concatErrors errsA errsB) where
          concatErrors :: [Position Error] -> [Position Error] -> [Position Error]
          concatErrors [Position 0 EmptyParser] errs = nub errs
          concatErrors errs [Position 0 EmptyParser] = nub errs
          concatErrors errsA' errsB' = nub (errsA' ++ errsB')

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser g where
  g (Position pos str) = case str of
    [] -> Failed [Position pos EndOfInput]
    (c:cs) -> if predicate c
      then Parsed c (Position (pos + 1) cs)
      else Failed [Position pos (Unexpected c)]
