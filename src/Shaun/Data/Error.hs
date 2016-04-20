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

module Shaun.Data.Error where
  import Shaun.Data.Type
  import Shaun.Syntax.Lexer

  -- |Data representing a token's type
  data TokenType =
      NumberToken
    | StringToken
    | BoolToken
    | SymbolToken String -- A specific symbol can be expected
    | SeparatorToken
    | IdentifierToken
    | Empty -- Used when end of file is encountered

  instance Show TokenType where
    show NumberToken = "a number"
    show StringToken = "a string"
    show BoolToken = "a boolean"
    show (SymbolToken s) = "symbol " ++ s
    show SeparatorToken = "a separator (newline or comma)"
    show IdentifierToken = "an identifier"
    show Empty = "end of stream"

  tokenType :: Token -> TokenType
  tokenType (Num _) = NumberToken
  tokenType (Str _) = StringToken
  tokenType (Boolean _) = BoolToken
  tokenType (Symbol s) = SymbolToken s
  tokenType Separator = SeparatorToken
  tokenType (Id _) = IdentifierToken

  -- |Data describing a SHAUN related error (ParsingError expected got)
  data Error =
    -- |LexicalError
      LexicalError String
    -- |Parsing error, when the token read is not the one expected
    | ParsingError TokenType TokenType
    -- |Multiple errors for multiple alternative parsers
    | AlternativeErrors [Error]
    -- |Type error, takes the expected type and the type of the faulty value
    | TypeError Type Type

  instance Show Error where
    show (LexicalError e) = e
    show (ParsingError ex got) = "expected " ++ show ex ++ " got " ++ show got
    show (AlternativeErrors errs) = foldr f (show $! last errs) (init errs)
      where
        f a b = show a ++ " or\n" ++ b
    show (TypeError ex got) = "expected type " ++ show ex ++ " got " ++ show got
