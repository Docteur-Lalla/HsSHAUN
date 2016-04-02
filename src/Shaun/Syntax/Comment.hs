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

module Shaun.Syntax.Comment (removeComments) where
  -- |Type of SHAUN comment
  data Comment = CSingleLine | CMultiLine | Parenthese

  -- |Function skipping characters depending on the comment type
  -- Returns Nothing if the end of the string is reached before the end of the comment
  skipComment :: Comment -> String -> Maybe String
  skipComment CSingleLine [] = Just [] -- End of string counts as end of comment for this one
  skipComment CSingleLine ('\n':xs) = Just xs
  skipComment CSingleLine (_:xs) = skipComment CSingleLine xs

  skipComment CMultiLine [] = Nothing
  skipComment CMultiLine ('*':'/':xs) = Just xs
  skipComment CMultiLine (_:xs) = skipComment CMultiLine xs

  skipComment Parenthese [] = Nothing
  skipComment Parenthese (')':xs) = Just xs
  skipComment Parenthese (_:xs) = skipComment Parenthese xs

  skipStringContents :: String -> Maybe (String, String)
  skipStringContents = skipStringContents' ""
    where
      skipStringContents' acc "" = Nothing
      skipStringContents' acc ('\\':'"':xs) = skipStringContents' (acc ++ "\\\"") xs
      skipStringContents' acc ('"':xs) = Just ('"':(acc ++ "\""), xs)
      skipStringContents' acc (x:xs) = skipStringContents' (acc ++ (x:"")) xs

  -- |Function going through the string to remove comments
  -- It returns Nothing if a C multiline or parentheses based comment is incomplete
  removeComments :: String -> Maybe String
  removeComments = removeComments' ""
    where
      skip :: Comment -> String -> String -> Maybe String
      skip com acc s =
        case rest of
          Just text -> removeComments' (newAcc com acc) text
          Nothing -> Nothing
        where
          rest = skipComment com s
          newAcc CSingleLine acc = acc ++ "\n"
          newAcc _ acc = acc

      removeComments' acc "" = Just acc
      removeComments' acc ('/':'/':xs) = skip CSingleLine acc xs
      removeComments' acc ('/':'*':xs) = skip CMultiLine acc xs
      removeComments' acc ('(':xs) = skip Parenthese acc xs
      removeComments' acc ('"':xs) =
        case skipStringContents xs of
          Nothing -> Nothing
          Just (s, tl) -> removeComments' (acc ++ s) tl
      removeComments' acc (x:xs) = removeComments' (acc ++ (x:"")) xs
