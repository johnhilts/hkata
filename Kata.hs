module Kata where

import Control.Category ((>>>))
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (sort)

-- | True if the string is an isogram
--
-- >>> isIsogram ""
-- True
--
-- >>> isIsogram "aa"
-- False
--
-- >>> isIsogram "aba"
-- False
--
-- >>> isIsogram "abb"
-- False
--
-- >>> isIsogram "abc"
-- True
--
-- >>> isIsogram "moOse"
-- False
--
-- >>> isIsogram "abca"
-- False
isIsogram :: String -> Bool
isIsogram = fmap toLower >>> sort >>> go
    where 
        go (a:b:rest)
            | a == b = False
            | otherwise = go (b:rest)
        go _ = True
