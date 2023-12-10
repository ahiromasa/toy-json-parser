module Main where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)

-- Parsers

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input1 -> do
    (input2, a) <- p input1
    Just (input2, f a)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)

  (Parser p1) <*> (Parser p2) = Parser $ \input1 -> do
    (input2, f) <- p1 input1
    (input3, a) <- p2 input2
    Just (input3, f a)

instance Alternative Parser where
  empty = Parser $ const Nothing

  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x : xs)
      | c == x = Just (xs, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input1 ->
  let (xs, input2) = span f input1
   in Just (input2, xs)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input1 -> do
  (input2, xs) <- p input1
  if null xs
    then Nothing
    else Just (input2, xs)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

ws :: Parser String
ws = spanP isSpace

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy separator element = (:) <$> element <*> many (separator *> element) <|> pure []

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  pure (snd <$> runParser parser input)

-- JSON

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = undefined

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where
    f xs = JsonNumber $ read xs

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = separateBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> separateBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

-- Main

main :: IO ()
main = do
  json <- parseFile "test.json" jsonValue
  print json
