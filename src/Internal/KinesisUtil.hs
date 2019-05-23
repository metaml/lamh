module Internal.KinesisUtil where

rekey :: String -> String
rekey "_data'" = "data"
rekey k = drop 1 k

rekey' :: String -> String
rekey' "_records" = "Records"
rekey' k = drop 1 k

rekey'' :: String -> String
rekey'' "_eventId" = "eventID"
rekey'' k = drop 1 k
