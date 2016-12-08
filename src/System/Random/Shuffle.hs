-- |
-- Module      : System.Random.Shuffle
-- Copyright   : (c) 2009 Oleg Kiselyov, Manlio Perillo
-- License     : BSD3 (see LICENSE file)
--
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
--
{-# OPTIONS_GHC -funbox-strict-fields #-}

module System.Random.Shuffle
    (
     shuffle
    , shuffle'
    ) where

import Data.Function (fix)
import System.Random (RandomGen, randomR)
import Control.Monad (liftM,liftM2)


-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a = Leaf !a
            | Node !Int !(Tree a) !(Tree a)
              deriving Show


-- Convert a sequence (e1...en) to a complete binary tree
buildTree :: [a] -> Tree a
buildTree = (fix growLevel) . (map Leaf)
    where
      growLevel _ [node] = node
      growLevel self l = self $ inner l

      inner [] = []
      inner [e] = [e]
      inner (e1 : e2 : es) = e1 `seq` e2 `seq` (join e1 e2) : inner es

      join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
      join l@(Node ct _ _)  r@(Leaf _)       = Node (ct + 1) l r
      join l@(Leaf _)       r@(Node ct _ _)  = Node (ct + 1) l r
      join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl + ctr) l r


-- |Given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.
shuffle :: [a] -> [Int] -> [a]
shuffle elements = shuffleTree (buildTree elements)
    where
      shuffleTree (Leaf e) [] = [e]
      shuffleTree tree (r : rs) =
          let (b, rest) = extractTree r tree
              in b : (shuffleTree rest rs)
      shuffleTree _ _ = error "[shuffle] called with lists of different lengths"

      -- Extracts the n-th element from the tree and returns
      -- that element, paired with a tree with the element
      -- deleted.
      -- The function maintains the invariant of the completeness
      -- of the tree: all internal nodes are always full.
      extractTree 0 (Node _ (Leaf e) r) = (e, r)
      extractTree 1 (Node 2 (Leaf l) (Leaf r)) = (r, Leaf l)
      extractTree n (Node c (Leaf l) r) =
        let (e, r') = extractTree (n - 1) r
        in (e, Node (c - 1) (Leaf l) r')

      extractTree n (Node n' l (Leaf e))
        | n + 1 == n' = (e, l)

      extractTree n (Node c l@(Node cl _ _) r)
        | n < cl = let (e, l') = extractTree n l
               in (e, Node (c - 1) l' r)
        | otherwise = let (e, r') = extractTree (n - cl) r
              in (e, Node (c - 1) l r')
      extractTree _ _ = error "[extractTree] impossible"

-- |Given a sequence (e1,...en) to shuffle, its length, and a random
-- generator, compute the corresponding permutation of the input
-- sequence.
shuffle' :: RandomGen gen => [a] -> gen -> ([a], gen)
shuffle' elements gen = (shuffle elements ids, last nextGen)
    where
      -- The sequence (r1,...r[n-1]) of numbers such that r[i] is an
      -- independent sample from a uniform random distribution
      -- [0..n-i]
      (ids, nextGen) = rseq (length elements) gen
      rseq :: RandomGen gen => Int -> gen -> ([Int], [gen])
      rseq n gen = unzip $ rseq' (n - 1) gen
          where
            rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
            rseq' 0 _ = []
            rseq' i gen = (j, gen) : rseq' (i - 1) gen'
                where
                  (j, gen') = randomR (0, i) gen
