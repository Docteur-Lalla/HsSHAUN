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

module Shaun.IO where
  import Shaun.Data.Type
  import Shaun.Data.Error
  import Shaun.Syntax.Lexer
  import Shaun.Syntax.Parser

  -- |Reads the specified file and parse SHAUN code
  -- This function returns a message if the reading or parsing fails
  parseShaunFromFile :: String -> IO (Either Error Object)
  parseShaunFromFile filename =
    do
      contents <- readFile filename
      putStrLn contents
      let lexer = makeLexer contents
      case lexer of
        Left e -> return (Left (LexicalError e))
        Right (_, stream) ->
          case parseShaunFile filename stream of
            Left e -> return (Left e)
            Right (_, v) -> return (Right v)

  -- |Writes SHAUN code of the given object into the specified file
  writeShaunToFile :: String -> Object -> IO ()
  writeShaunToFile filename = writeFile filename . dedent . removeBrackets . show
    where
      removeBrackets = unlines . init . tail . lines
      dedent = unlines . map removeSpaces . lines
      removeSpaces (' ':' ':xs) = xs
      removeSpaces l = l
