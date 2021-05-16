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
import qualified Random as R

main = do
  gen <- newStdGen
  -- ^ get the first seed

  let ptPlans = [rhythmPlan, patternPlan, transformPlan]
  -- ^ get the plans for each pt

  let (cps,gen2) = runState (R.getRandoms 2 chordProgressions) gen
  let chordsTree = toMT $ insertionsPT cps
  -- ^ the starting tree, filled with chords

  let (final, gen3) = sequencePlans gen2 ptPlans chordsTree
  -- ^ the final tree, with all pts applied to it.

  playDev 6 $ treeToMusic final

-- generative material: --------------------------------------------------------

rhythmPlan = Plan { _ttPool = map rhythm' [ronettes, n, evn 4]
                  , _ttDepth = \tree -> measureDepth tree + 1
                  , _ptShape = shape
                  }

patternPlan = Plan { _ttPool = map pattern [full, waltz, sadwaltz]
                  ,  _ttDepth = \tree -> measureDepth tree
                  ,  _ptShape = shape
                  }

transformPlan = Plan { _ttPool = [inv, rev, transp (-1), strong, weak]
                  ,  _ttDepth = \tree -> measureDepth tree - 1
                  ,  _ptShape = shape
                  }

shape = SNode [SLeaf 2]

structured :: Orientation -> [[Primitive Pitch]] -> MusicOT
structured o chords = Group H $ map (toGroup o) chords

insertionsPT cs = Node (atDepth 0 [0..1]) [
              Node (atDepth 1 [0..2]) [
                Leaf (atDepth 2 [0..1]) (insert $ structured V (cs !! 0))
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
