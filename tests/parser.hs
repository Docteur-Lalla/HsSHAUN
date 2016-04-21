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

import Shaun.Data.Type
import Shaun.Data.Error
import Shaun.Syntax.Token
import Shaun.Syntax.Lexer
import Shaun.Syntax.Parser

import System.Environment

code = "obj: {" ++
  "int: 64 m\n" ++
  "list: [ 78 km, \"yolo\", true ]\n" ++
  "tree: { a: 5\nb: 1 rad }\n}\n\ni:42 life\n" ++
  "objb: { int: 2 }"

int = "4.5e2 rad "
str = "\"hehe\" "
bool = "true"
simple_li = "[ 1, 2, 3, 4, 5 ]"
li = "[ \"abc\", 1, 3, 4 km, \"def\",  5, 6, 7, true, \"ghi\" ]"
tr = "{ obj: 5 } "

main =
  do
    case makeLexer li of
      Left e -> putStrLn e
      Right (_, s) ->
        case parseValue s of
          Left e -> putStrLn (show e)
          Right (_, v) -> putStrLn (show v)
    case makeLexer code of
      Left e -> putStrLn e
      Right (_, s) ->
        case parseShaunFile "" s of
          Left e -> putStrLn (show e)
          Right (_, v) -> putStrLn (show v)
    args <- getArgs
    case length args of
      1 ->
        do
          let filename = args !! 0
          contents <- readFile filename
          let lexer = makeLexer contents
          case lexer of
            Left e -> putStrLn e
            Right (_, stream) ->
              case parseShaunFile filename stream of
                Left e -> putStrLn (show e)
                Right (_, v) -> putStrLn (show v)
      _ -> return ()
