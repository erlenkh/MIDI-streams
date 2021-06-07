module Generation
( sequencePlans
, Plan (..)
, Shape (..)
, measureDepth
) where

import MusicTrees
import Trees
import qualified Random as R

import Control.Monad.State
import System.Random

-- GENERATING MUSIC PTS : ------------------------------------------------------

-- the meta model for each PT:
data Plan = Plan { _ttPool :: [MusicOT -> MusicOT]
                  -- ^ the list of possible tree transformations
                  , _ttDepth :: MusicOT -> Int
                  --   ^ depth at which the tt's should be applied in OT
                  , _ptShape :: Shape
                 }

-- the shape of each PT
data Shape = SLeaf Int | SNode [Shape] deriving Show -- problematic?
-- what if a node contains 2 Vals and 1 Group?
-- Ans: shape is just used a skeleton to make prefixTrees,
-- so it is ok that these trees are somewhat constrained.
-- (might actually ditch this for just defaultPT as part of plan)

defaultPT :: Shape -> MusicPT
defaultPT (SLeaf n) = Node id (replicate n $ Leaf id (id))
defaultPT (SNode shapes) = Node id (map defaultPT shapes)
-- |                ^ id is default function. doesnt change anything..

defaultPT' = Node id [
              Leaf id id
            , Leaf id id
            , Leaf id id
            ]

defaultPT'' = Node id [
                  Node id [
                    Leaf id id
                  , Leaf id id
                  ]
                  Node id [
                    Leaf id id
                  , Leaf id id
                  ]
              ]

-- generates a random PT according to the plan and OT it is to be applied to:
genPT :: StdGen -> Plan -> MusicOT -> (MusicPT, StdGen)
genPT gen plan ot =
  let dpt = defaultPT'' -- (_ptShape plan)
      (sts, gen2) = randomDFSTs gen (keysAmt dpt) ot (_ttDepth plan ot)
      (tts, gen3) = randomTTs gen2 (valuesAmt dpt) (_ttPool plan)
  in  (elevateValues tts $ elevate sts $ dpt, gen3)

-- gets the depth at which the measure is at. By definition:
measureDepth :: MusicOT -> Int
measureDepth tree = (height tree) - 3

randomTTs :: StdGen -> Int -> [MusicOT -> MusicOT] -> ([MusicOT -> MusicOT], StdGen)
randomTTs gen n tts = runState (R.getRandoms n tts) gen
-- | gets n random tree transformations from list of tts.

-- GENERATING SEQUENCES OF PTS -------------------------------------------------

sequencePlans :: StdGen -> [Plan] -> MusicOT -> (MusicOT, StdGen)
sequencePlans gen plans startTree =
  foldl (\(tree, gen) plan -> nextOT gen plan tree) (startTree, gen) plans

nextOT :: StdGen -> Plan -> MusicOT -> (MusicOT, StdGen)
nextOT gen ptPlan otree =
  let (newPT, gen2) = genPT gen ptPlan otree
  in (applyPT newPT otree, gen2)

-- RANDOM SLICE TRANSFORMATIONS: -----------------------------------------------

-- problem: some prefixtrees dont adress all levels, and thus might result
-- in a new musicOT with a non-constant depth. This is bad!

-- random depth-fixed slice transformations:
-- results in a pt where all TT are applied at the same given depth.
randomDFSTs
  :: StdGen -> Int -> OrientedTree a -> Int -> ([Slice -> Slice], StdGen)
randomDFSTs gen n ot depth =
  let (dfst, gen2) = randomDFST gen ot depth
      (sts, gen3) = randomSTs gen2 (n - 1) ot [0..depth]
  in (dfst : sts, gen3)

-- random depth-fixed slice transformation:
randomDFST :: StdGen -> OrientedTree a -> Int -> (Slice -> Slice, StdGen)
randomDFST gen tree depth =
  let ([child], gen2) = randomChildren gen [depth] tree
  in (st depth child, gen2)

randomSTs :: StdGen -> Int -> OrientedTree a -> [Int] -> ([Slice -> Slice], StdGen)
randomSTs gen n ot depthRange =
  let (depths, gen2) = randomDepths gen n depthRange
      (children, gen3) = randomChildren gen depths ot
  in (zipWith st depths children, gen3)
  -- | gets n random slice transformations based on shape of oriented tree.

st :: Int -> Int -> (Slice -> Slice)
st depth child = atDepth depth [0..child]

randomDepths :: StdGen -> Int -> [Int] -> ([Int], StdGen)
randomDepths gen n range = runState (R.getRandoms n range) gen
-- | ^ gets n random depths from a given range

randomChildren :: StdGen -> [Int] -> OrientedTree a -> ([Int], StdGen)
randomChildren gen depths ot =
  let childRanges = map (childRange ot) depths
  in runState (R.getRandomss childRanges) gen
  -- | ^ gets a random child (within the ot) for each depth given as input.

-- range of accessable depth levels in tree:
depthRange :: OrientedTree a -> [Int]
depthRange tree = [0 .. (height tree) - 2] --  -1 bc of 0-index

-- range of accessable children-indexes in tree at a given depth:
childRange :: OrientedTree a -> Int -> [Int]
childRange tree depth = [0 .. (minimum $ childrenAtDepth tree depth) - 1] -- 0-index
-- ^ min due to slicing, (+ the amt of children at a depth is mostly constant)

-- TESTING: --------------------------------------------------------------------
shape :: Shape
shape = SNode [SLeaf 2]
