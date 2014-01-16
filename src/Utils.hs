module Utils where

count :: Eq a => a -> [a] -> Int
count x list = length $ filter (==x) list

for = flip map
