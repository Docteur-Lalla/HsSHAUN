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
import Shaun.Syntax.Parser
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as BS

code = "obj: {" ++
  "int: 64 m\n" ++
  "list: [ 78 km, \"yolo\", true ]\n" ++
  "tree: { a: 5\nb: 1 rad }\n}\n\ni:42 life\n" ++
  "objb: { int: 2 }"

int = "4.5e2 rad "
str = "\"hehe\" "
bool = "true"
li = "[ \"abc\", 1, 3, 4, \"def\",  5, 6, 7, true, \"ghi\" ]"
tr = "{ obj: 5 } "

main =
  do
    case shaunResult $ eitherResult $! (parse parseNumber (BS.pack int)) of
      Left (TypeError exp got) -> putStrLn ("expected type " ++ show exp ++ " got " ++ show got)
      Left (ParsingError err) -> putStrLn ("Parsing number : " ++ err)
      Right val -> putStrLn ("Number : " ++ show val)
    case shaunResult $ eitherResult $! (parse parseString (BS.pack str)) of
      Left (TypeError exp got) -> putStrLn ("expected type " ++ show exp ++ " got " ++ show got)
      Left (ParsingError err) -> putStrLn ("Parsing string : " ++ err)
      Right val -> putStrLn ("String data : " ++ show val)
    case shaunResult $ eitherResult $! (parse parseBoolean (BS.pack bool)) of
      Left (TypeError exp got) -> putStrLn ("expected type " ++ show exp ++ " got " ++ show got)
      Left (ParsingError err) -> putStrLn ("Parsing boolean : " ++ err)
      Right val -> putStrLn ("Boolean : " ++ show val)
    case shaunResult $ eitherResult $! (parse parseList (BS.pack li)) of
      Left (TypeError exp got) -> putStrLn ("expected type " ++ show exp ++ " got " ++ show got)
      Left (ParsingError err) -> putStrLn ("Parsing list : " ++ err)
      Right val -> putStrLn ("List : " ++ show val)
    case shaunResult $ eitherResult $! (parse parseTree (BS.pack tr)) of
      Left (TypeError exp got) -> putStrLn ("expected type " ++ show exp ++ " got " ++ show got)
      Left (ParsingError err) -> putStrLn ("Parsing tree : " ++ err)
      Right val -> putStrLn ("Tree : " ++ show val)
    case parseShaunCode code of
      Left (TypeError exp got) -> putStrLn ("expected type " ++ show exp ++ " got " ++ show got)
      Left (ParsingError err) -> putStrLn ("Parsing complete code : " ++ err)
      Right val -> putStrLn ("Complete data : " ++ show val)
