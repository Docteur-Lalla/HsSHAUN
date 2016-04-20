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
  import Shaun.Syntax.Lexer

  type Parser a = [Token] -> Either Error ([Token], a)

  -- |Looks for a number at the head of the stream
  parseNumber :: Parser Object
  parseNumber [] = Left (ParsingError NumberToken Empty)
  parseNumber (Num val:Id unit:xs) = Right $! (xs, number val (Just unit))
  parseNumber (Num val:xs) = Right $! (xs, number val Nothing)
  parseNumber (got:_) = Left (ParsingError NumberToken (tokenType got))

  -- |Looks for a boolean at the head of the stream
  parseBoolean :: Parser Object
  parseBoolean [] = Left (ParsingError BoolToken Empty)
  parseBoolean (Boolean val:xs) = Right $! (xs, boolean val)
  parseBoolean (got:_) = Left (ParsingError BoolToken (tokenType got))

  -- |Looks for a string at the head of the stream
  parseString :: Parser Object
  parseString [] = Left (ParsingError StringToken Empty)
  parseString (Str val:xs) = Right $! (xs, string val)
  parseString (got:_) = Left (ParsingError StringToken (tokenType got))

  -- |Parses many occurences of the given parser
  many :: Parser a -> Parser [a]
  many = many' []
    where
      many' acc _ [] = Right $! ([], reverse acc)
      many' acc p s =
        case p s of
          Left _ -> Right $! (s, reverse acc)
          Right (ss, val) -> many' (val:acc) p ss

  -- |Parses many occurences of the second parser, separated by the first one
  -- No occurence is actually needed
  sepBy :: Parser a -> Parser b -> Parser [b]
  sepBy sep p s =
    case p s of
      Left _ -> Right $! (s, [])
      Right (ss, val) -> sepBy' [val] ss
    
    where
      sepBy' acc [] = Right $! ([], reverse acc)
      sepBy' acc s' =
        case sep s' of
          Left _ -> Right $! (s', reverse acc)
          Right (ss', _) ->
            case p ss' of
              Left _ -> Right $! (s', reverse acc)
              Right (sss', val') -> sepBy' (val':acc) sss'

  -- |Tries each parser and returns the result of the first successing
  alt :: [Parser a] -> Parser a
  alt ps s = alt' [] ps s
    where
      alt' errs [] _ = Left (AlternativeErrors errs)
      alt' errs (p:ps') s' =
        case p s' of
          Left e -> alt' (e:errs) ps' s
          r@(Right _) -> r

  or :: Parser a -> Parser a -> Parser a
  or a b = alt [a, b]

  -- |Parses a separator (newline or comma)
  parseSeparator :: Parser ()
  parseSeparator [] = Left (ParsingError SeparatorToken Empty)
  parseSeparator (Separator:xs) = Right $! (xs, ())
  parseSeparator (got:_) = Left (ParsingError SeparatorToken (tokenType got))

  -- |Parses any SHAUN value
  parseValue :: Parser Object
  parseValue = alt [parseNumber, parseString, parseBoolean, parseList, parseTree]

  -- |Parses a list of values
  parseList :: Parser Object
  parseList [] = Left (ParsingError (SymbolToken "[") Empty)
  parseList (Symbol "[":xs) =
    do
      (s', _) <- many parseSeparator xs
      (ss', vals) <- sepBy (many parseSeparator) parseValue s'
      (sss', _) <- many parseSeparator ss'
      case sss' of
        [] -> Left (ParsingError (SymbolToken "]") Empty)
        (Symbol "]":ssss') -> Right $! (ssss', ListObj vals)
        (got:_) -> Left (ParsingError (SymbolToken "]") (tokenType got))
  parseList (got:_) = Left (ParsingError (SymbolToken "[") (tokenType got))

  parsePair :: Parser (String, Object)
  parsePair [] = Left (ParsingError IdentifierToken Empty)
  parsePair (Id i:Symbol ":":xs) =
    do
      (s', val) <- parseValue xs
      Right $! (s', (i, val))
  parsePair (Id _:got:_) = Left (ParsingError (SymbolToken ":") (tokenType got))
  parsePair (got:_) = Left (ParsingError IdentifierToken (tokenType got))

  parseTree :: Parser Object
  parseTree [] = Left (ParsingError (SymbolToken "{") Empty)
  parseTree s =
    do
      (s', _) <- many parseSeparator s
      case s' of
        [] -> Left (ParsingError (SymbolToken "{") Empty)
        (Symbol "{":ss') ->
          do
            (sss', _) <- many parseSeparator ss'
            (ssss', pairs) <- sepBy (many parseSeparator) parsePair sss'
            (sssss', _) <- many parseSeparator ssss'
            case sssss' of
              [] -> Left (ParsingError (SymbolToken "}") Empty)
              (Symbol "}":ssssss') -> Right $! (ssssss', TreeObj pairs)
              (got:_) ->  Left (ParsingError (SymbolToken "}") (tokenType got))
        (got:_) -> Left (ParsingError (SymbolToken "{") (tokenType got))
  
  parseShaunFile :: String -> Parser Object
  parseShaunFile _ stream =
    do
      (s', _) <- many parseSeparator stream
      (ss', obj) <- parseTree (Symbol "{":s'++[Symbol "}"])
      (sss', _) <- many parseSeparator ss'
      case sss' of
        [] -> Right $! ([], obj)
        (got:_) -> Left (ParsingError Empty (tokenType got))
