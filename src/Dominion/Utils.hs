module Dominion.Utils where
import Control.Monad
import Control.Monad.State
import Data.Random.Extras
import Data.Random hiding (shuffle)
import System.Random
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
deckShuffle deck = do
    gen <- getStdGen
    let (shuffled, newGen) = sampleState (shuffle deck) gen
    setStdGen newGen
    return shuffled

-- times :: Monad m => Int -> m a -> [m b]
times iterations block = forM_ [1..iterations] $ \_ -> block

indices :: [a] -> [Int]
indices arr = [0..(length arr - 1)]

join = intercalate


failIf :: Bool -> String -> Either String ()
failIf True str = Left str
failIf False str = Right ()
