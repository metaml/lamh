module Main where

import Control.Monad
import Lambda

main :: IO ()
main = forever echoEventIO
