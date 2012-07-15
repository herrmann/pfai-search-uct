-- |
-- Module      :  AI.Search.UCT
-- Copyright   :  (c) 2012, Ricardo Herrmann 
-- License     :  BSD-style
-- Maintainer  :  rherrmann@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of the Upper Confidence bounds applied to Trees (UCT) algorithm using a zipper data structure for maintaining and updating the tree. Based on the code from <http://www.mcts.ai/?q=code/simple_java>.

module AI.Search.UCT (
  -- * Types
  UctNode
  , visits
  , value
  , FullUctNode
  -- * Auxiliary
  , emptyUctNode
  , emptyLoc
  -- * Algorithm
  , selectAction
  -- ** Secondary
  , update
  , select
  , uctValue
  , trial
  -- ** Testing
  , uct
  , randomExpand
  , randomRollOut
  -- * Debugging
  , printTree
  ) where

import Data.Maybe (fromJust)
import Data.List (foldl')

import Data.Tree
import Data.Tree.Zipper

import Control.Monad (foldM,liftM)
import Control.Monad.Random (MonadRandom,getRandom,getRandomR)

-- | UCT tree node.
data UctNode = UctNode {
  visits :: {-# UNPACK #-} !Int,   -- ^ number of times this node was visited
  value  :: {-# UNPACK #-} !Double -- ^ the upper confidence bound of the value
  } deriving Show

-- | Type synonym for full UCT nodes inside the rose tree zipper
type FullUctNode = TreePos Full UctNode

emptyUctNode = UctNode 0 0.0

emptyLoc :: FullUctNode
emptyLoc = fromTree $ Node emptyUctNode []

epsilon = 0.000001

-- | Runs a number of UCT trials.
uct :: MonadRandom m
  => Int           -- ^ Number of trials
  -> FullUctNode   -- ^ Tree root location
  -> m FullUctNode -- ^ New tree root location
uct numTrials loc = foldM (flip ($)) loc actions
  where
    actions = replicate numTrials $ selectAction randomExpand randomRollOut

-- | Does one round of UCT.
-- MonadRandom is used to break ties randomly in unexpanded nodes.
selectAction :: MonadRandom m
  => (FullUctNode -> m FullUctNode) -- ^ Node expansion function
  -> (FullUctNode -> m Double)      -- ^ Rollout function
  -> FullUctNode                    -- ^ Tree root location
  -> m FullUctNode                  -- ^ New tree root location
selectAction expand rollOut loc = do
  leaf <- trial loc
  leaf' <- expand leaf
  newNode <- select leaf'
  val <- rollOut newNode
  let top = update val newNode
  return top

-- | Selects the best node down the tree according to the UCT rule.
trial :: MonadRandom m
  => FullUctNode   -- ^ Tree root location
  -> m FullUctNode -- ^ Location of leaf chosen by UCT
trial loc
  | isLeaf loc = return loc
  | otherwise = do
    s <- select loc
    trial s

-- | Selects the node with best UCT value. Assumes there's at least one child node.
select :: MonadRandom m
  => FullUctNode   -- ^ Location of parent node
  -> m FullUctNode -- ^ Location of child node with best UCT value
select loc = do
  let firstNode = fromJust $ firstChild loc
  firstVal <- uctVal firstNode
  go firstNode firstVal (next firstNode)
  where
    uctVal node = uctValue logVisits $ label node
    locVisits = visits $ label loc
    logVisits = log (fromIntegral locVisits + 1)
    go bestNode _ Nothing = return bestNode
    go bestNode bestVal (Just newNode) = do
      newVal <- uctVal newNode
      let nextNode = next newNode
      if newVal > bestVal
        then go newNode newVal nextNode
        else go bestNode bestVal nextNode

-- | Computes the UCT value of a node.
{-# INLINE uctValue #-}
uctValue :: MonadRandom m
  => Double   -- ^ log(parent node visits + 1)
  -> UctNode  -- ^ UCT node info
  -> m Double -- ^ UCT value of the node
uctValue logVisits n = do
  c <- getRandom
  let val = a + b + c * epsilon
  return $! val
  where
    a = value n / (v + epsilon)
    b = sqrt (logVisits / (v + epsilon))
    v = fromIntegral $ visits n

-- | Updates node values from leaf to root
update :: Double -- ^ The value from the rollout
  -> FullUctNode -- ^ Leaf node location
  -> FullUctNode -- ^ Updated tree root location
update val = go
  where
    go loc' =
      let loc'' = modifyLabel upd loc'
          upd n = UctNode { visits = visits n + 1, value = value n + val }
      in case parent loc'' of
        Nothing -> loc''
        Just l -> go l

-- | Dummy rollout function that chooses between victory or defeat at random.
randomRollOut :: MonadRandom m => FullUctNode -> m Double
randomRollOut loc = liftM fromIntegral (getRandomR (0,1 :: Int))

-- | Expands a tree node with a random number of children from 2 to 5.
randomExpand :: MonadRandom m
  => FullUctNode -- ^ Location of parent node
  -> m FullUctNode -- ^ Location of expanded parent node
randomExpand loc = do
  n <- getRandomR (2,5)
  return . fromJust . parent $ last n
  where
    last n = foldl' f loc (zip (replicate n emptyUctNode) moves)
    f l (label, move) = insert (Node label []) (move l)
    moves = children : repeat nextSpace

-- | Prints the tree from a node location in the zipper.
printTree :: Show a => TreePos Full a -> IO ()
printTree = putStrLn . drawTree . fmap show . tree

