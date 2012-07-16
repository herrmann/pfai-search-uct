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
  , main
  -- * Debugging
  , printTree
  ) where

import Data.Maybe (fromJust,isJust)
import Data.List (foldl')

import Data.Tree
import Data.Tree.Zipper

import Control.Monad (foldM,liftM)
import Control.Monad.Primitive (PrimMonad,PrimState)

import System.Random.MWC

import System.Environment (getArgs)

-- For testing purposes
main = do
  args <- getArgs
  let trials = read $ head args
  tree <- withSystemRandom . asGenST $ \gen -> run gen trials
  print . value $ label tree
  where
    run gen trials = uct gen trials emptyLoc

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
uct :: PrimMonad m
  => Gen (PrimState m) -- ^ Random number generator
  -> Int               -- ^ Number of trials
  -> FullUctNode       -- ^ Tree root location
  -> m FullUctNode     -- ^ New tree root location
uct gen numTrials loc = foldM (flip ($)) loc actions
  where
    actions = replicate numTrials $ selectAction gen randomExpand randomRollOut

-- | Does one round of UCT.
-- The random number generator is used to break ties randomly in unexpanded nodes.
selectAction :: PrimMonad m
  => Gen (PrimState m)                                   -- ^ Random number generator
  -> (Gen (PrimState m) -> FullUctNode -> m FullUctNode) -- ^ Node expansion function
  -> (Gen (PrimState m) -> FullUctNode -> m Double)      -- ^ Rollout function
  -> FullUctNode                                         -- ^ Tree root location
  -> m FullUctNode                                       -- ^ New tree root location
selectAction gen expand rollOut loc = do
  leaf <- trial gen loc
  leaf' <- expand gen leaf
  newNode <- select gen leaf'
  val <- rollOut gen newNode
  let top = update val newNode
  return top

-- | Selects the best node down the tree according to the UCT rule.
trial :: PrimMonad m
  => Gen (PrimState m) -- ^ Random number generator
  -> FullUctNode       -- ^ Tree root location
  -> m FullUctNode     -- ^ Location of leaf chosen by UCT
trial gen loc = go loc
  where
    go loc
      | isLeaf loc = return loc
      | otherwise = do
        loc' <- select gen loc
        go loc'

-- | Selects the node with best UCT value. Assumes there's at least one child node.
select :: PrimMonad m
  => Gen (PrimState m) -- ^ Random number generator
  -> FullUctNode       -- ^ Location of parent node
  -> m FullUctNode     -- ^ Location of child node with best UCT value
select gen loc = do
  nodeValues <- childrenValues gen loc
  best <- bestNode nodeValues
  return $! node best

data NodeValue = NodeValue { node :: !FullUctNode, val :: !Double }

bestNode :: Monad m => [NodeValue] -> m NodeValue
bestNode = foldM1 cmp

childrenValues gen loc = mapM eval $ locChildren loc
  where
    uctVal = uctValue gen parentVisits . label
    parentVisits = log (fromIntegral (visits $ label loc) + 1)
    eval loc = do
      value <- uctVal loc
      return $! NodeValue loc value

{-# INLINE locChildren #-}
locChildren = map fromJust . takeWhile isJust . iterate maybeNext . firstChild

cmp n1@(NodeValue _ v1) n2@(NodeValue _ v2)
  | v1 > v2   = return $! n1
  | otherwise = return $! n2

foldM1 _ [] = error "foldM1" "empty list"
foldM1 f (x:xs) = foldM f x xs

maybeNext Nothing = Nothing
maybeNext (Just loc) = next loc

-- | Computes the UCT value of a node.
{-# INLINE uctValue #-}
uctValue :: PrimMonad m
  => Gen (PrimState m) -- ^ Random number generator
  -> Double            -- ^ log(parent node visits + 1)
  -> UctNode           -- ^ UCT node info
  -> m Double          -- ^ UCT value of the node
uctValue gen logVisits n = do
  c <- uniform gen
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
randomRollOut :: (Num a, PrimMonad m) => Gen (PrimState m) -> t -> m a
randomRollOut gen loc = liftM fromIntegral (uniformR (0,1 :: Int) gen)

-- | Expands a tree node with a random number of children from 2 to 5.
randomExpand :: PrimMonad m
  => Gen (PrimState m) -- ^ Random number generator
  -> FullUctNode -- ^ Location of parent node
  -> m FullUctNode -- ^ Location of expanded parent node
randomExpand gen loc = do
  n <- uniformR (2,5) gen
  return . fromJust . parent $ last n
  where
    last n = foldl' f loc (zip (replicate n emptyUctNode) moves)
    f l (label, move) = insert (Node label []) (move l)
    moves = children : repeat nextSpace

-- | Prints the tree from a node location in the zipper.
printTree :: Show a => TreePos Full a -> IO ()
printTree = putStrLn . drawTree . fmap show . tree

