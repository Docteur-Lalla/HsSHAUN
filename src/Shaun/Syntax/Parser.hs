{-
  Copyright (c) 2016, Kévin Le Bon
  All rights reserved.
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
  * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
  * Neither the name of Kévin Le Bon nor the names of its contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

module Shaun.Syntax.Parser (parseShaunFile, parseShaunCode) where
  import qualified Shaun.Data.Type as ShaunType
  import Shaun.Syntax.Comment
  import Text.Parsec hiding (spaces)
  import Text.Parsec.String

  -- |Function scanning blank characters (newline is not allowed)
  blank :: Parser Char
  blank = oneOf " \t,"

  spaces :: Parser Char
  spaces = blank <|> newline

  -- |Parser for numbers
  parseNumber :: Parser Double
  parseNumber =
    do
      val <- parse
      return (read val)
    where
      parse =
        do
          sign <- option '0' (char '-')
          full <- many1 digit
          dec <- option "" $ do
            char '.'
            val <- many1 digit
            return ('.':val)
          exp <- option "" $ do
            e <- oneOf "Ee"
            sign <- option '0' (char '+' <|> char '-')
            val <- many1 digit
            return (e:sign:val)
          return (sign:(full ++ dec ++ exp))

  -- |Parser for numbers with unit
  parseNumberWithUnit :: Parser (Double, Maybe String)
  parseNumberWithUnit =
    do
      num <- parseNumber
      skipMany blank
      unit <- try (optionMaybe (many1 letter))
      return (num, unit)

  -- |Parser for booleans
  parseBoolean :: Parser Bool
  parseBoolean =
    do
      val <- (string "true" <|> string "false")
      return $ case val of
        "true" -> True
        "false" -> False

  -- |Parser for strings
  parseString :: Parser String
  parseString =
    do
      char '"'
      val <- many (parseEscape <|> noneOf "\"")
      char '"'
      return val
    where
      parseEscape =
        do
          char '\\'
          c <- oneOf "\\nrt\""
          return $ case c of
            '\\' -> '\\'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            '"' -> '"'

  -- |Parser for list of values
  parseList :: Parser [ShaunType.Object]
  parseList =
    do
      skipMany spaces
      char '['
      skipMany spaces
      elems <- listElems `sepEndBy` (many spaces)
      skipMany spaces
      char ']'
      return elems
    where
      listElems =
        do
          skipMany spaces
          val <- parseShaunValue
          skipMany spaces
          return val

  -- |Parser for attribute bindings like attribute_name: "value"
  parseShaunAttribute :: Parser (String, ShaunType.Object)
  parseShaunAttribute =
    do
      skipMany blank
      name <- many (letter <|> digit <|> char '_')
      skipMany blank
      char ':'
      skipMany blank
      val <- parseShaunValue
      skipMany blank
      return (name, val)

  -- |Parser for SHAUN trees
  parseShaunTree :: Parser ShaunType.Object
  parseShaunTree =
    do
      skipMany spaces
      char '{'
      skipMany spaces
      pairs <- parseShaunAttribute `sepEndBy` (many1 sep)
      skipMany spaces
      char '}'
      skipMany blank
      return (ShaunType.tree pairs)
    where sep = newline >> many spaces

  parseShaunList = fmap ShaunType.list parseList
  parseShaunNumber = fmap number parseNumberWithUnit
    where number (n, u) = ShaunType.number n u
  parseShaunBoolean = fmap ShaunType.boolean parseBoolean
  parseShaunString = fmap ShaunType.string parseString

  -- |Parser for SHAUN values
  parseShaunValue :: Parser ShaunType.Object
  parseShaunValue = parseShaunNumber
    <|> parseShaunBoolean
    <|> parseShaunString
    <|> parseShaunList
    <|> parseShaunTree

  -- |Parser for a complete SHAUN code retrieved from a file
  parseShaunFile :: String -> String -> Either String ShaunType.Object
  parseShaunFile filename code =
    case clean_code of
      Nothing -> Left "could not parse end of comment"
      Just text -> case parse parseShaunTree filename ("{" ++ text ++ "}") of
        Left err -> Left (show err)
        Right val -> Right val
    where
      clean_code = removeComments code

  -- |Parser for a complete SHAUN code
  parseShaunCode :: String -> Either String ShaunType.Object
  parseShaunCode = parseShaunFile ""
