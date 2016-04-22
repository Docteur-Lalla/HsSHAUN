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

-- |Module implementing a sweeper for SHAUN object trees.
-- The sweeper takes a SHAUN tree and a path (Haskell String) and try to retrieve
-- the value in the corresponding tree.
module Shaun.Data.Sweeper (computePath) where
  import Shaun.Data.Type
  import Shaun.Data.Error
  import Shaun.Syntax.Token
  import Shaun.Syntax.Lexer

  getNextNode :: Object -> [Token] -> Either Error ([Token], Object)
  getNextNode tree (Id name:Symbol "[":Num n:Symbol "]":xs) =
    case attr of
      Just (ListObj l) -> Right (xs, l !! idx)
      Just obj -> Left (TypeError ListType (typeOf obj))
      Nothing -> Left (AttributeNotFound name)
    where
      attr = tree `getAttribute` name
      idx = floor n
  getNextNode tree (Id name:xs) =
    case tree `getAttribute` name of
      Just obj -> Right (xs, obj)
      Nothing -> Left (AttributeNotFound name)

  getValue :: Object -> [Token] -> Either Error Object
  getValue tree path =
    case getNextNode tree path of
      Left e -> Left e
      Right ([], v) -> Right v
      Right (Symbol ":":xs, node) -> getValue node xs
      Right (tok:_, _) -> Left (ParsingError (SymbolToken ":") (tokenType tok))

  -- |get the value defined by the given path in the given tree object
  computePath :: Object -> String -> Either Error Object
  computePath t@(TreeObj _) path =
    case makeLexer path of
      Left e -> Left (LexicalError e)
      Right (_, lexer) -> getValue t lexer
  computePath _ "" = Left (LexicalError "unexpected empty path")
  computePath obj _ = Left (TypeError ObjectType (typeOf obj)) 
