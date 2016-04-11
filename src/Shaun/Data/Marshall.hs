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
  import Shaun.Data.Error

  class Shaun a where
    -- |Encode the given data to a SHAUN object
    encode :: a -> Object
    -- |Retrieve data from a SHAUN object
    decode :: Object -> Either Error a

  instance Shaun Double where
    encode d = number d Nothing

    decode (NumberObj (d, _)) = Right d
    decode v = Left (TypeError NumberType (typeOf v))

  instance Shaun Bool where
    encode b = boolean b

    decode (BoolObj b) = Right b
    decode v = Left (TypeError BoolType (typeOf v))

  instance Shaun a => Shaun [a] where
    encode l = list (map encode l)

    decode (ListObj l) = format [] tmp
      where
        tmp = map decode l
        format acc [] = Right acc
        format acc (Left err:xs) = Left err
        format acc (Right x:xs) = format (acc ++ [x]) xs
    decode v = Left (TypeError ListType (typeOf v))

  instance Shaun String where
    encode s = string s

    decode (StringObj s) = Right s
    decode v = Left (TypeError StringType (typeOf v))

  instance Shaun a => Shaun (Maybe a) where
    encode (Just v) = list [encode v]
    encode Nothing = list []

    decode (ListObj [v]) = case decode v of
      Right m -> Right (Just m)
      Left err -> Left err
    decode (ListObj []) = Right Nothing
    decode v = Left (TypeError ListType (typeOf v))

  instance (Shaun a, Shaun b) => Shaun (Either a b) where
    encode (Left a) = tree [("left", encode a)]
    encode (Right b) = tree [("right", encode b)]

    decode (TreeObj [ei]) = case ei of
      ("left", a) -> case decode a of
        Right va -> Right (Left va)
        Left err -> Left err 
      ("right", b) -> case decode b of
        Right vb -> Right (Right vb)
        Left err -> Left err
    decode v = Left (TypeError ObjectType (typeOf v))
