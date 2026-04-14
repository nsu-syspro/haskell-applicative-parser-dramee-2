{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import Data.Char (toLower, isDigit)
import Data.List (intercalate)
import ParserCombinators (char, string)
import Control.Applicative ((<|>), optional, Alternative (some), many)
import Data.Maybe
import Data.Functor (($>))

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = ws *> value <* ws

value :: Parser JValue
value =
      parseNull
  <|> parseBool
  <|> parseNumber
  <|> parseString
  <|> parseArray
  <|> parseObject

-- null
parseNull :: Parser JValue
parseNull = JNull <$ string "null"

-- bool
parseBool :: Parser JValue
parseBool =
      (JBool True <$ string "true")
  <|> (JBool False <$ string "false")

-- number (с дробью и экспонентой)
parseNumber :: Parser JValue
parseNumber = do
  JNumber . read <$> numberString

numberString :: Parser String
numberString =
  (\sign intPart fracPart expPart -> concat $
    [maybe "" (:[]) sign, intPart]
    ++ maybeToList fracPart
    ++ maybeToList expPart)
  <$> optional (char '-')
  <*> int
  <*> optional fraction
  <*> optional exponentPart

int :: Parser String
int =
      (:) <$> char '0' <*> pure ""
  <|> (:) <$> satisfy (\c -> c >= '1' && c <= '9') <*> many (satisfy isDigit)

fraction :: Parser String
fraction =
  (:) <$> char '.' <*> some (satisfy isDigit)

exponentPart :: Parser String
exponentPart =
  (\e sign digits -> e : maybe "" (:[]) sign ++ digits)
    <$> satisfy (\c -> c == 'e' || c == 'E')
    <*> optional (satisfy (\c -> c == '+' || c == '-'))
    <*> some (satisfy isDigit)

-- string (с escape)
parseString :: Parser JValue
parseString =
  JString . concat <$>
    (char '"' *> many stringChunk <* char '"')

stringChunk :: Parser String
stringChunk =
      escapeChunk
  <|> ((:[]) <$> satisfy (\c -> c /= '"' && c /= '\\'))

escapeChunk :: Parser String
escapeChunk =
  (\c -> ['\\', c]) <$>
    (char '\\' *> satisfy (`elem` "\"\\/bfnrt"))

-- array
parseArray :: Parser JValue
parseArray = JArray <$> (char '[' *> ws *> elements <* ws <* char ']')

elements :: Parser [JValue]
elements =
      (value `sepBy` (ws *> char ',' <* ws))
  <|> pure []

-- object
parseObject :: Parser JValue
parseObject = JObject <$> (char '{' *> ws *> members <* ws <* char '}')

members :: Parser [(String, JValue)]
members =
      (pair `sepBy` (ws *> char ',' <* ws))
  <|> pure []

pair :: Parser (String, JValue)
pair =
  (\keyVal _ val -> case keyVal of
      JString key -> (key, val)
      _ -> error "parseString should always return JString")
    <$> parseString
    <*> (ws *> char ':' *> ws)
    <*> value

-- whitespace
ws :: Parser ()
ws = many (satisfy (`elem` " \n\r\t")) $> ()

-- sepBy (если нет в Parser.hs)
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
      (:) <$> p <*> many (sep *> p)
  <|> pure []

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
