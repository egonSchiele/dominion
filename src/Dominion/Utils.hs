module Dominion.Utils where
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Lazy as M
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle
import Language.Haskell.HsColour.ANSI
import Data.List

red = highlight [Foreground Red]
green = highlight [Foreground Green]
yellow = highlight [Foreground Yellow]
blue = highlight [Foreground Blue]
cyan = highlight [Foreground Cyan]
dim = highlight [Dim]

count :: Eq a => a -> [a] -> Int
count x list = length $ filter (==x) list

countBy func list = length $ filter func list

for = flip map

deckShuffle :: [a] -> IO [a]
deckShuffle deck = Random.getStdRandom $ Shuffle.shuffle' deck

-- times :: Monad m => Int -> m a -> [m b]
times iterations block = forM_ [1..iterations] $ const block

indices :: [a] -> [Int]
indices arr = [0..(length arr - 1)]

join = intercalate

failIf :: Bool -> String -> Either String ()
failIf True str = Left str
failIf False str = Right ()

-- | rotate a list
--
-- >>> rotate 2 [1, 2, 3]
-- [3, 2, 1]
--
-- >>> rotate 6 [1, 2, 3]
-- [1, 2, 3]
--
rotate :: Int -> [a] -> [a]
rotate n xs = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

decrement :: Ord a => a -> M.Map a Int -> M.Map a Int
decrement = M.adjust (\x -> x - 1)
