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

import Shaun.Data.Type
import Shaun.Syntax.Comment
import Shaun.Syntax.Lexer
import Shaun.Syntax.Parser
import Shaun.Data.Sweeper

code = "obj: {" ++
  "int: 64 m\n" ++
  "list: [ 78 km, \"yolo\", true ]\n" ++
  "tree: { a: 5\nb: 1 rad }\n}\n\ni:42 life\n" ++
  "objb: { int: 2 }"

main =
  case makeLexer code of
    Left e -> putStrLn (show e)
    Right (_, lexer) ->
      case parseShaunFile "" lexer of
        Left e -> putStrLn (show e)
        Right (_, v) ->
          case computePath v "obj:list[1]" of
            Left e -> putStrLn (show e)
            Right v -> putStrLn (show v)
