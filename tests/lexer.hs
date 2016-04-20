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

import Shaun.Syntax.Lexer

number :: IO ()
number =
  do
    case readNumber "123" of
      Left s -> putStrLn s
      Right (_, Num i) -> putStrLn $ show i
    case readNumber "-123.23" of
      Left s -> putStrLn s
      Right (_, Num i) -> putStrLn $ show i
    case readNumber "123e3" of
      Left s -> putStrLn s
      Right (_, Num i) -> putStrLn $ show i
    case readNumber "123.5e-4" of
      Left s -> putStrLn s
      Right (_, Num i) -> putStrLn $ show i

string :: IO ()
string =
  do
    case readString "\"Das war ein Befehl\"" of
      Left s -> putStrLn s
      Right (_, Str s) -> putStrLn ('"':s ++ "\"")

identifier :: IO ()
identifier =
  do
    case readIdentifier "_ab12cd" of
      Left s -> putStrLn s
      Right (_, Id i) -> putStrLn i
    case readIdentifier "ab_12cd" of
      Left s -> putStrLn s
      Right (_, Id i) -> putStrLn i
    case readIdentifier "Ab12cd_" of
      Left s -> putStrLn s
      Right (_, Id i) -> putStrLn i

completeCode =
  "obj: {\n" ++
  "  i: 23.5e-3 rad\n" ++
  "  b: true\n" ++
  "}"

main =
  do
    number
    string
    identifier
    case makeLexer completeCode of
      Left s -> putStrLn s
      Right ("", v) -> putStrLn $! foldr f "" v
        where
          f a b = show a ++ b
