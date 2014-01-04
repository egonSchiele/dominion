module Player where
import qualified Card as C
data Player = Player {
                name :: String,
                deck :: [C.Card],
                discard :: [C.Card]
} deriving Show
