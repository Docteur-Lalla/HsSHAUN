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

module Shaun.Syntax.Parser where
  import qualified Shaun.Data.Type as ShaunType
  import Shaun.Syntax.Comment
  import Text.ParserCombinators.Parsec hiding (spaces)

  -- |Parser for numbers
  parseNumber :: Parser Double
  parseNumber =
    do
      val <- parse
      return (read val)
    where
      parse =
        do
          full <- many1 (oneOf "1234567890")
          dec <- option "" $ do
            char '.'
            val <- many1 (oneOf "1234567890")
            return ('.':val)
          exp <- option "" $ do
            e <- oneOf "Ee"
            sign <- option '+' (char '-')
            val <- many1 (oneOf "1234567890")
            return (e:sign:val)
          return (full ++ dec ++ exp)

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
