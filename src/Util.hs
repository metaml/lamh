module Util where

import Data.Text

showt :: Show a => a -> Text
showt = pack . show
