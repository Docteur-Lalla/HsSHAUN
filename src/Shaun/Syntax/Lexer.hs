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

module Shaun.Syntax.Lexer (
  readBoolean,
  readNumber,
  readString,
  readIdentifier,
  makeLexer,
  Lexer,
  Token (Num, Str, Boolean, Id, Symbol, Separator)) where
  import Data.Char

  -- |Token type
  data Token =
      Num Double
    | Str String
    | Boolean Bool
    | Id String
    | Symbol String
    | Separator

  instance Show Token where
    show (Num d) = show d
    show (Str s) = s
    show (Boolean b) = show b
    show (Id i) = i
    show (Symbol s) = s
    show Separator = ","

  -- |Lexer type
  type Lexer a = Either String (String, a)

  -- |Reads a boolean in the input stream
  readBoolean :: String -> Lexer Token
  readBoolean ('t':'r':'u':'e':xs) = Right (xs, Boolean True)
  readBoolean ('f':'a':'l':'s':'e':xs) = Right (xs, Boolean False)
  readBoolean _ = Left "expected 'true' or 'false'"

  -- |Reads a number in the input stream
  readNumber :: String -> Lexer Token
  readNumber (str@(c:cs)) =
    do
      let sign = if c == '-' then -1.0 else 1.0
      let s = if sign > 0 then str else cs
      (ss, val) <- readNumber' s
      case val of
        Num n -> Right (ss, Num (sign * n))
        _ -> Left "expected a digit"
  readNumber "" = Left "expected a digit"

  -- Reads a positive real number
  readNumber' :: String -> Lexer Token
  readNumber' "" = Left "expected a digit"
  readNumber' s =
    case i of
      "" -> Left "expected a digit" 
      _ -> Right $! (ssss, Num (read (i ++ dec ++ ex)))

    where
      (ss, i) =
        case readInteger "" s of
        Nothing -> (s, "")
        Just (s', int) -> (s', int)
      (sss, dec) =
        case readDecimalPart ss of
          Nothing -> (ss, "")
          Just (s', dec') -> (s', ('.':dec'))
      (ssss, ex) =
        case readExpPart sss of
          Nothing -> (sss, "")
          Just (s', ex') -> (s', ('e':ex'))
      
      -- Reads a list of digits (integral part of a number)
      readInteger :: String -> String -> Maybe (String, String)
      readInteger "" "" = Nothing
      readInteger acc "" = Just ("", reverse acc)
      readInteger acc (c':cs)
        | isDigit c' = readInteger (c':acc) cs
        | otherwise = Just (c':cs, reverse acc)

      -- Reads the decimal part of the number
      readDecimalPart :: String -> Maybe (String, String)
      readDecimalPart "" = Nothing
      readDecimalPart ('.':str@(c':_))
        | isDigit c' = readInteger "" str
        | otherwise = Nothing
      readDecimalPart _ = Nothing
      
      -- Reads the exponent part of the number
      readExpPart :: String -> Maybe (String, String)
      readExpPart "" = Nothing
      readExpPart (e':s':cs)
        | e' == 'e' || e' == 'E' =
          case s' of
            '+' -> readInteger "" cs
            '-' ->
              case readInteger "" cs of
                Nothing -> Nothing
                Just (ss', val) -> Just (ss', '-':val)
            _ -> readInteger "" (s':cs)
        | otherwise = Nothing
      readExpPart _ = Nothing

  -- |Reads a string and escape special characters
  readString :: String -> Lexer Token
  readString ('"':s) = readStringContents "" s
    where
      readStringContents _ "" = Left "expected end of string (symbol \")"
      readStringContents acc ('"':cs) = Right (cs, Str $! reverse acc)
      readStringContents acc ('\\':c:cs) = readStringContents (nc:acc) cs
        where
          nc =
            case c of
              '\\' -> '\\'
              'n' -> '\n'
              't' -> '\t'
              'r' -> '\r'
              _ -> c
      readStringContents acc (c:cs) = readStringContents (c:acc) cs
  readString _ = Left "expected symbol \""

  -- |Reads an identifier (same rules as C's identifiers)
  readIdentifier :: String -> Lexer Token
  readIdentifier "" = Left "expected letter or underscore"
  readIdentifier (c:cs)
    | isAlpha c || c == '_' = readIdentifier' [c] cs
    | otherwise = Left "expected letter or underscore"
    where
      readIdentifier' acc "" = Right $! ("", Id $! reverse acc)
      readIdentifier' acc (c':cs')
        | isAlphaNum c' || c' == '_' = readIdentifier' (c':acc) cs'
        | otherwise = Right (c':cs', Id $! reverse acc)
  
  -- |Reads the entire code and make a lexer from it
  makeLexer :: String -> Lexer [Token]
  makeLexer "" = Right ("", [])
  makeLexer string = makeLexer' [] string
    where
      -- Lists of special characters (spaces and symbols)
      spaces = " \t"
      symbols = "{}[]:"

      -- Skip every consecutive blanks
      skipBlanks "" = ""
      skipBlanks (str@(c:cs))
        | c `elem` spaces = skipBlanks cs
        | otherwise = str
    
      makeLexer' acc "" = Right $! ("", reverse acc)
      makeLexer' acc (str@(c:cs))
        -- c is a blank
        | c `elem` spaces = makeLexer' acc (skipBlanks str)
        -- c is a symbol
        | c `elem` symbols = makeLexer' (Symbol [c] : acc) cs
        -- c begins a string
        | c == '"' =
          case readString str of
            Left s -> Left s
            Right (ss, v) -> makeLexer' (v : acc) ss
        -- c is a separator
        | c == '\n' || c == ',' = makeLexer' (Separator : acc) cs
        -- c begins a number
        | isDigit c || c == '-' =
          case readNumber str of
            Left s -> Left s
            Right (ss, v) -> makeLexer' (v : acc) ss
        -- c begins either a boolean or an identifier
        | isAlpha c || c == '_' =
          case readBoolean str of
            -- looks for a boolean first
            Right (ss, v) -> makeLexer' (v : acc) ss
            Left s ->
              -- if it Lefts, looks for an identifier
              case readIdentifier str of
                Left s2 -> Left (s ++ " or " ++ s2)
                Right (ss, v) -> makeLexer' (v : acc) ss
        | otherwise = Left ("unexpected character " ++ [c])
