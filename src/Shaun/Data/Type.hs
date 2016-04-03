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

module Shaun.Data.Type where
  -- |SHAUN data type
  data Object = NumberObj (Double, Maybe String)
    | BoolObj Bool
    | StringObj String
    | ListObj [Object]
    | TreeObj [(String, Object)]

  -- |helper function to create SHAUN numbers
  number :: Double -> Maybe String -> Object
  number f unit = NumberObj (f, unit)

  -- |helper function to create SHAUN booleans
  boolean :: Bool -> Object
  boolean = BoolObj

  -- |helper function to create SHAUN string
  string :: String -> Object
  string = StringObj

  -- |helper function to create SHAUN lists
  list :: [Object] -> Object
  list = ListObj

  -- |helper function to create SHAUN trees
  tree :: [(String, Object)] -> Object
  tree = TreeObj

  -- |Predicate returning True if the given Object is a number having a unit
  hasUnit :: Object -> Bool
  hasUnit (NumberObj (_, Nothing)) = False
  hasUnit (NumberObj (_, Just _)) = True
  hasUnit _ = False

  -- |Function returning the unit of a SHAUN number having one
  getUnit :: Object -> Maybe String
  getUnit (NumberObj (_, Just unit)) = Just unit
  getUnit _ = Nothing

  -- |Predicate checking if a SHAUN tree has the given child object
  hasAttribute :: String -> Object -> Bool
  hasAttribute _ (TreeObj []) = False
  hasAttribute child (TreeObj ((name, _):xs))
    | (child == name) = True
    | otherwise = hasAttribute child (TreeObj xs)
  hasAttribute _ _ = False

  -- |Function returning the specified child object of a tree object
  getAttribute :: String -> Object -> Maybe Object
  getAttribute _ (TreeObj []) = Nothing
  getAttribute child (TreeObj ((name, obj):xs))
    | (child == name) = Just obj
    | otherwise = getAttribute child (TreeObj xs)
  getAttribute _ _ = Nothing

  -- |Function returning the nth element of a list object
  getListElem :: Int -> Object -> Maybe Object
  getListElem _ (ListObj []) = Nothing
  getListElem n (ListObj l@(x:xs))
    | length l <= n = Nothing
    | otherwise = Just (l !! n)
  getListElem _ _ = Nothing

  instance Show Object where
    show (NumberObj (num, Nothing)) = show num
    show (NumberObj (num, Just unit)) = show num ++ " " ++ unit
    show (StringObj s) = "\"" ++ s ++ "\""
    show (BoolObj b) = show b
    show (ListObj []) = "[]"
    show (ListObj l@(x:xs)) = case any pred l of
      False -> "[" ++ foldl (\a b -> a ++ ", " ++ show b) (show x) xs ++ "]"
      True -> "[" ++ indent (foldl (\a b -> a ++ "\n" ++ b) "" lin) ++ "]"
        where
          sl = foldl (\a b -> a ++ "\n" ++ b) (show x) (map show xs)
          lin = lines sl
      where
        pred (ListObj _) = True
        pred (TreeObj _) = True
        pred (StringObj _) = True
        pred _ = False

        indent s = unlines (map (\line -> "  " ++ line) (lines s))
    show (TreeObj t) = "{" ++ indent (foldl show_tree "" t) ++ "}"
      where
        show_tree a (name, val) = a ++ "\n" ++ name ++ ": " ++ show val
        indent s = unlines (map (\line -> "  " ++ line) (lines s))
