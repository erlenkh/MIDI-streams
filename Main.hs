{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}
import System.Random
import Composer
import Structure
import qualified Transform as T
import Euterpea
import Chord
import qualified Data.List as L
import Control.Monad.State
import Scale

main = do
  gen <- newStdGen

  -- getPlans (random or predefined)

  -- foldically apply each plan. to get final OT.

  -- play it.
  
  let (cps,gen2) = runState (getRandoms 2 chordProgressions) gen
--  let (rs, gen3) = runState (getRandoms 2 rhythms) gen2
--  let (ps, gen4) = runState (getRandoms 2 patterns) gen3
  let inserted = toMT $ insertionsPT cps
  let rhythmed = applyPT (genPT gen rhythmPlan inserted) inserted
  let patterned = applyPT (genPT gen patternPlan rhythmed) rhythmed

  putStrLn (show $ genPT gen patternPlan rhythmed)
  playDev 6 $ treeToMusic patterned

nextOtree gen ptPlan otree = applyPT (genPT gen ptPlan otree) otree

inserted = toMT $ insertionsPT chordProgressions
rhythmed gen = applyPT (genPT gen rhythmPlan inserted) inserted
pt_random gen =(genPT gen rhythmPlan inserted)

-- prefix trees: ---------------------------------------------------------------

structured :: Orientation -> [[Primitive Pitch]] -> MusicOT
structured o chords = Group H $ map (toGroup o) chords

insertionsPT cs = Node (atDepth 0 [0..1]) [
              Node (atDepth 1 [0..2]) [
                Leaf (atDepth 2 [0..1]) (insert $ structured V (cs !! 0 ))
              , Leaf (atDepth 2 [2..3]) (insert $ structured V (cs !! 1))
              ]
            ]

insPT cs = Node (atDepth 0 [0]) [
              Node (atDepth 1 [0..1]) [
                Leaf (atDepth 2 [0..1]) (insert $ structured V (cs !! 0 ))
              , Leaf (atDepth 2 [2..3]) (insert $ structured V (cs !! 1))
              ]
            ]

-- selected chords/patterns/rhythms:
chordProgressions = [nico, house, dreams]
rhythms = [n, ronettes, ffff, evn 4]
patterns = [full, sad, falling, waltz, rising, sadwaltz]

--basic chord progressions:
mkc p o m ints dur = map (\p -> Note dur p) $ pitches $ getChord (p,o) m ints

nico = [mkc C 3 Major [2,4,6] wn, mkc F 3 Major [2,4,6] wn]
house = [mkc A 3 Minor [2,4,6] wn, mkc E 3 Minor [2,4,6] wn]
dreams = [mkc C 3 Major [2,4,6] wn, mkc A 2 Minor [2,4,6] wn]
strange = [mkc D 3 Major [2,4,6] wn, mkc D 3 Minor [2,4,6] wn]

-- basic patterns:
full, falling, waltz, rising :: Pattern
full = [[0,1,2]]
sad = [[0,1,2,3]]
falling = [[2], [1], [0]]
waltz = [[0], [1,2], [1,2]]
sadwaltz = [[0,3], [1,2,3], [1,2,3]]
rising = L.reverse falling

-- basic rhythms:
n = [(qn + en), (qn + en), qn]
ronettes = [(qn + en), en, hn]
ffff = [(qn +en), en, en, en, qn]
mr = [(qn + en), (qn + en), qn,(qn + en), qn, (qn + en)]

-- RANDOM: ---------------------------------------------------------------------

getRandoms :: Int -> [a] -> State StdGen [a]
getRandoms x list = sequence $ replicate x (randomSt list)

randomSt :: (RandomGen g) => [a] -> State g a
randomSt list = state $ getRandom list

getRandom :: (RandomGen g) => [a] -> g -> (a, g)
getRandom list seed =
  let randomIdx = randomR (0,(length list) -1) seed
  in (list !! fst randomIdx, snd randomIdx)


-- EXPERIMENTATION ZONE: -------------------------------------------------------

-- this zone is for experimenting on generating prefix trees.

-- for NOW: in OT each level the nodes have the same amount of members?

-- general patttern : genPT ::  Plan -> MusicOT -> MusicPT

data Plan = Plan { _ttPool :: [MusicOT -> MusicOT]
                  , _ttDepth :: MusicOT -> Int
                  --   ^ depth at which the tt's should be applied in OT
                  , _ptShape :: Shape
                 }

rhythmPlan = Plan { _ttPool = map rhythm [n, ronettes, evn 4]
                  , _ttDepth = \tree -> measureDepth tree + 1
                  , _ptShape = shape
                  }

patternPlan = Plan { _ttPool = map pattern [full, rising, falling]
                  ,  _ttDepth = \tree -> measureDepth tree
                  ,  _ptShape = shape
                  }


-- map applySF [T.invert C Major, T.transpose C Major 2, T.transpose C Major 1]
data Shape = SLeaf Int | SNode [Shape] deriving Show -- problematic?
-- what if a node contains 2 Vals and 1 Group?
-- Ans: shape is just used a skeleton to make prefixTrees,
-- so it is ok that these trees are somewhat constrained.
-- (might actually ditch this for just defaultPT as part of plan)

sDepth :: Shape -> Int
sDepth (SLeaf x) = 1
sDepth (SNode shapes) = 1 + maximum (map sDepth shapes)
-- ^ tree denoting shape of a tree x.
-- Only SLeaves need to say how many Leaves x has.
-- For SNode, this number is the length of its list of members.

shape = SNode [SLeaf 2]

sm = atDepth 0 [1]
tt = id

defaultPT :: Shape -> MusicPT
defaultPT (SLeaf n) = Node id (replicate n $ Leaf id (id))
defaultPT (SNode shapes) = Node id (map defaultPT shapes)
-- |                ^ id is default function. doesnt change anything..


-- GEN MEANS RANDOM!
genPT :: StdGen -> Plan -> (MusicOT) -> MusicPT
genPT gen plan oTree =
  let dpt = defaultPT (_ptShape plan)
      (sliceTs, gen2) = randomDFSTs gen (keysAmt dpt) oTree (_ttDepth plan oTree)
      (treeTs, gen3) = randomTTs gen2 (valuesAmt dpt) (_ttPool plan)
  in  elevateValues treeTs $ elevateKeys sliceTs $ dpt

--
randomTTs :: StdGen -> Int -> [MusicOT -> MusicOT] -> ([MusicOT -> MusicOT], StdGen)
randomTTs gen n tts = runState (getRandoms n tts) gen
-- | gets n random tree transformations from list of tts.


-- PROBLEM: pattern function only works at lower levels.
-- MANY FUNCtioNS will be like this. only works on groups of vals.
-- easy to fix this with randomSTSAFE or something like this.
-- generates randomSTs, but replaces the first with an ST that adresses the
-- depth that is the next lowest!


-- other problem: some prefixtrees dont adress all levels, and thus might result
-- in a new musicOT with a non-constant depth. This is bad!

-- random depth-fixed slice transformations:
randomDFSTs :: StdGen -> Int -> OrientedTree a -> Int -> ([Slice -> Slice], StdGen)
randomDFSTs gen n ot depth =
  let dfst = randomDFST gen ot depth
      sts = randomSTs' gen (n - 1) ot [0..depth]
  in (fst dfst : fst sts, snd sts)

-- random depth-fixed slice transformation:
randomDFST :: StdGen -> OrientedTree a -> Int -> (Slice -> Slice, StdGen)
randomDFST gen tree depth =
  let ([width], gen2) = randomWidths gen [depth] tree
  in (atDepth depth [0..width], gen2)

measureDepth tree = (depth tree) - 4

randomSTs :: StdGen -> Int -> OrientedTree a -> ([Slice -> Slice], StdGen)
randomSTs gen n ot = randomSTs' gen n ot (depthRange ot)

randomSTs' :: StdGen -> Int -> OrientedTree a -> [Int] -> ([Slice -> Slice], StdGen)
randomSTs' gen n ot depthRange =
  let (depths, gen2) = randomDepths gen n depthRange
      (widths, gen3) = randomWidths gen depths ot
  in (zipWith (\d w -> atDepth d [0..w]) depths widths, gen3)
  -- | gets n random slice transformations based on shape of oriented tree.

randomDepths :: StdGen -> Int -> [Int] -> ([Int], StdGen)
randomDepths gen n range = runState (getRandoms n range) gen
-- | ^ gets n random depths from a given range

randomWidths :: StdGen -> [Int] -> OrientedTree a -> ([Int], StdGen)
randomWidths gen depths ot =
  runState (sequence $ map (randomSt . widthRange ot) depths) gen
  -- | ^ gets a random width for each depth given as input.
