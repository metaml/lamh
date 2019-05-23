module Internal.SnsUtil where

import Data.Char

rekey :: String -> String
rekey "_snsType" = "Type"
rekey "_attributeType" = "Type"
rekey v  = capitalize . drop 1 $ v

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs
