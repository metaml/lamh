module Internal.S3Util where

rekey :: String -> String
rekey "_s3object" = "object"
rekey k = drop 1 k
