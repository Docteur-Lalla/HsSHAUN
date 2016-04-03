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

{-# LANGUAGE FlexibleInstances #-}

module Shaun.Data.Marshall where
  import Shaun.Data.Type
  import Control.Monad

  class Shaun a where
    -- |Encode the given data to a SHAUN object
    encode :: a -> Object
    -- |Retrieve data from a SHAUN object
    decode :: Object -> Maybe a

  instance Shaun Double where
    encode d = number d Nothing

    decode (NumberObj (d, _)) = Just d
    decode _ = Nothing

  instance Shaun Bool where
    encode b = boolean b

    decode (BoolObj b) = Just b
    decode _ = Nothing

  instance Shaun Char where
    encode c = string [c]

    decode (StringObj [c]) = Just c
    decode _ = Nothing

  instance Shaun a => Shaun [a] where
    encode l = list (map encode l)

    decode (ListObj l) = format [] tmp
      where
        tmp = map decode l
        format acc [] = Just acc
        format acc (Nothing:xs) = Nothing
        format acc (Just x:xs) = format (acc ++ [x]) xs
    decode _ = Nothing

  instance Shaun String where
    encode s = string s

    decode (StringObj s) = Just s
    decode _ = Nothing

  instance Shaun a => Shaun (Maybe a) where
    encode (Just v) = list [encode v]
    encode Nothing = list []

    decode (ListObj [v]) = case decode v of
      Just m -> Just (Just m)
      Nothing -> Nothing
    decode (ListObj []) = Just Nothing
    decode _ = Nothing

  instance (Shaun a, Shaun b) => Shaun (Either a b) where
    encode (Left a) = tree [("left", encode a)]
    encode (Right b) = tree [("right", encode b)]

    decode (TreeObj [either]) = case either of
      ("left", a) -> case decode a of
        Just va -> Just (Left va)
        Nothing -> Nothing
      ("right", b) -> case decode b of
        Just vb -> Just (Right vb)
        Nothing -> Nothing
    decode _ = Nothing
