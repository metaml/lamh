module Internal.EventUtil where

rekey :: String -> String
rekey "_eventId" = "eventID"
rekey k = drop 1 k
