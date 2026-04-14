{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import Control.Applicative ((<|>), some)
import Data.Char (digitToInt, isDigit)
import ParserCombinators (char, string)
import Task1 (digitsToInt)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = dotFormat <|> hyphenFormat <|> usFormat

dotFormat :: Parser Date
dotFormat = Date <$> day <* char '.' <*> month <* char '.' <*> year

hyphenFormat :: Parser Date
hyphenFormat = Date <$> day <* char '-' <*> month <* char '-' <*> year

usFormat :: Parser Date
usFormat = flip Date <$> monthName <* char ' ' <*> usDay <* char ' ' <*> year


day :: Parser Day
day = char '0' *> (Day <$> nonZeroDigit) <|> char '1' *> (Day . (10+) <$> digit) <|> char '2' *> (Day . (20+) <$> digit) <|> Day . fromIntegral . digitsToInt <$> string "30" <|> Day . fromIntegral . digitsToInt <$> string "31"

usDay :: Parser Day
usDay =  char '1' *> (Day . (10+) <$> digit) <|> char '2' *> (Day . (20+) <$> digit) <|> Day . fromIntegral . digitsToInt <$> string "30" <|> Day . fromIntegral . digitsToInt <$> string "31" <|> (Day <$> nonZeroDigit)


month :: Parser Month
month = char '0' *> (Month <$> nonZeroDigit) <|> Month . fromIntegral . digitsToInt <$> string "10" <|> Month . fromIntegral . digitsToInt <$> string "11" <|> Month . fromIntegral . digitsToInt <$> string "12"


year :: Parser Year
year = Year <$> number

monthName :: Parser Month
monthName = strToMonth <$> string "Jan" <|> strToMonth <$> string "Feb" <|> strToMonth <$> string "Mar" <|> strToMonth <$> string "Apr" <|> strToMonth <$> string "May" <|> strToMonth <$> string "Jun" <|> strToMonth <$> string "Jul" <|> strToMonth <$> string "Aug" <|> strToMonth <$> string "Sep" <|> strToMonth <$> string "Oct" <|> strToMonth <$> string "Nov" <|> strToMonth <$> string "Dec"

strToMonth :: String -> Month
strToMonth s = Month $ case s of
  "Jan" -> 1
  "Feb" -> 2
  "Mar" -> 3
  "Apr" -> 4
  "May" -> 5
  "Jun" -> 6
  "Jul" -> 7
  "Aug" -> 8
  "Sep" -> 9
  "Oct" -> 10
  "Nov" -> 11
  "Dec" -> 12
  _ -> error "Invalid month name"

nonZeroDigit :: Parser Int
nonZeroDigit = digitToInt <$> satisfy (\ c -> c >= '1' && c <= '9')

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

number :: Parser Int
number = foldl (\ acc d -> acc * 10 + d) 0 <$> some digit
