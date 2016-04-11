{-
  Copyright (c) 2016, Kévin Le Bon
  
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
  * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  
  * Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.
  
  * Neither the name of Kévin Le Bon nor the names of other
  contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

module Shaun.Syntax.Parser where
  import Shaun.Data.Type
  import Shaun.Data.Error
  import Shaun.Syntax.Comment
  import Data.Attoparsec.ByteString as Atto
  import Data.Word
  import qualified Data.ByteString.Char8 as BS
  import qualified Data.ByteString.Internal as BS (c2w, w2c)

  spaces :: Parser ()
  spaces = skipWhile space_predicate
    where space_predicate = inClass " \t"

  blanks :: Parser Word8
  blanks = choice [ char ' ', char '\t', char '\n' ]

  char :: Char -> Parser Word8
  char = word8 . BS.c2w

  separator :: Parser ()
  separator =
    choice [hardNewline, withComma]
    where
      withComma =
        do
          newlines
          comma
          newlines
      hardNewline =
        do
          spaces
          char '\n'
          newlines
      
      comma = skip (\w -> w == BS.c2w ',')
      newlines = skipWhile (inClass " \t\n")

  -- |Parser for numbers with or without unit
  parseNumber :: Parser Object
  parseNumber =
    do
      sign <- option (BS.c2w '0') (char '-')
      val <- takeWhile1 (inClass "0-9")
      dec <- option ".0" decimal
      e <- option "e0" exponant
      let str = (BS.w2c sign) : (BS.unpack val ++ dec ++ e)
      u <- option Nothing unit
      return $ NumberObj $! (read str, u)
    where
      decimal =
        do
          char '.'
          dec <- takeWhile1 (inClass "0-9")
          return $! ('.' : BS.unpack dec)
      exponant =
        do
          satisfy (inClass "eE")
          s <- option (BS.c2w '0') (choice [char '+', char '-'])
          e <- takeWhile1 (inClass "0-9")
          let sign = if BS.w2c s == '+' then '0' else BS.w2c s
          return $! ('e' : sign : BS.unpack e)
      unit =
        do
          spaces
          u <- takeWhile1 (inClass "a-zA-Z")
          return $! (Just (BS.unpack u))
  
  -- |Parser for booleans
  parseBoolean :: Parser Object
  parseBoolean = choice [parseTrue, parseFalse]
    where
      parseTrue = Atto.string (BS.pack "true") >> return (BoolObj True)
      parseFalse = Atto.string (BS.pack "false") >> return (BoolObj False)

  -- |Parser for strings
  parseString :: Parser Object
  parseString =
    do
      char '"'
      str <- many' (choice [parseEscape, notWord8 (BS.c2w '"')])
      char '"'
      return $! (StringObj (map BS.w2c str))
    where
      parseEscape =
        do
          char '\\'
          c <- choice (map char "\\nrt\"")
          return $! BS.c2w $ case BS.w2c c of
            '\\' -> '\\'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            '"' -> '"'

  -- |Parser for a list of SHAUN objects
  parseList :: Parser Object
  parseList =
    do
      skipMany blanks
      char '['
      skipMany blanks
      elems <- parseShaunValue `sepBy` separator
      skipMany blanks
      char ']'
      return $! (ListObj elems)

  -- |Parser for a tree
  parseTree :: Parser Object
  parseTree =
    do
      skipMany blanks
      char '{'
      skipMany blanks
      elems <- parseShaunPair `sepBy` separator
      skipMany blanks
      char '}'
      return $! (TreeObj elems)
    where
      parseShaunPair =
        do
          ident <- takeWhile1 (inClass "a-zA-Z_0-9")
          spaces
          char ':'
          spaces
          val <- parseShaunValue
          return $! (BS.unpack ident, val)

  -- |Parser for any SHAUN value
  parseShaunValue :: Parser Object
  parseShaunValue = choice
    [
      parseBoolean,
      parseList,
      parseTree,
      parseNumber,
      parseString
    ]

  -- |Transforms an Either String Object to a Either Error Object
  shaunResult :: Either String Object -> Either Error Object
  shaunResult (Left err) = Left (ParsingError err)
  shaunResult (Right val) = Right val

  -- |Parser for a complete SHAUN code retrieved from a file
  parseShaunFile :: String -> String -> Either Error Object
  parseShaunFile filename code =
    case clean_code of
      Nothing -> Left (ParsingError (filename ++ "could not parse end of comment"))
      Just text -> case shaunResult . eitherResult $! parsing_result text of
        Left err -> Left err
        Right val -> Right val
    where
      parsing_result text = parse parseTree $! (BS.pack ("{" ++ text ++ "}"))
      clean_code = removeComments code

  -- |Parser for a complete SHAUN code
  parseShaunCode :: String -> Either Error Object
  parseShaunCode = parseShaunFile ""
