module Example
( genPiece
) where

import MusicTrees
import Trees
import Transform
import Chord
import Scale
import Generation
import qualified Random as R

import Euterpea
import System.Random
import Control.Monad.State
import qualified Data.List as L

-- The area where you can specify how the example piece is generated:
genPiece :: StdGen -> Euterpea.Music (Pitch, Volume)
genPiece gen =
  let (cps,gen2) = genChordProgs 2 gen
      chordsTree = genStartingTree cps
      -- ^ the starting tree, filled with chords
      (final, gen3) = sequencePlans gen2 ptPlans chordsTree
      -- ^ the final tree, with all pts applied to it.

  in toMusic final

-- GENERATIVE PLANS: -----------------------------------------------------------

ptPlans = [rhythmPlan, patternPlan]

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

-- STARTING CHORD TREE: --------------------------------------------------------

structured :: Orientation -> [[Primitive Pitch]] -> MusicOT
structured o chords = Group H $ map (toGroup o) chords

genStartingTree cps = toMT $ insertionsPT cps

insertionsPT cs = Node (atDepth 0 [0..1]) [
              Node (atDepth 1 [0..2]) [
                Leaf (atDepth 2 [0..1]) (insert $ structured V (cs !! 0))
              , Leaf (atDepth 2 [2..3]) (insert $ structured V (cs !! 1))
              ]
            ]

insertionsPT' = Node (atDepth 0 [0]) [
                  Leaf (atDepth 1 [0,1]) (insert $ structured V nico)
                ]

testBoy = toMT insertionsPT'

chordProgressions = [nico, house, dreams]

genChordProgs x = runState (R.getRandoms x chordProgressions)

--chord progression pool:
mkc p o m ints dur = map (\p -> Note dur p) $ pitches $ getChord (p,o) m ints

nico = [mkc C 3 Major [2,4,6] wn, mkc F 3 Major [2,4,6] wn]
house = [mkc A 3 Minor [2,4,6] wn, mkc E 3 Minor [2,4,6] wn]
dreams = [mkc C 3 Major [2,4,6] wn, mkc A 2 Minor [2,4,6] wn]
strange = [mkc D 3 Major [2,4,6] wn, mkc D 3 Minor [2,4,6] wn]

-- TREE TRANSFORMATIONS: -------------------------------------------------------

-- patterns pool:
patterns = [full, sad, falling, waltz, rising, sadwaltz]

full, sad, falling, waltz, sadwaltz, rising :: Pattern
full = [[0,1,2]]
sad = [[0,1,2,3]]
falling = [[2], [1], [0]]
waltz = [[0], [1,2], [1,2]]
sadwaltz = [[0,3], [1,2,3], [1,2,3]]
rising = L.reverse falling

-- rhythm pool:
rhythms = [n, ronettes, ffff, evn 4]

n = [(qn + en), (qn + en), qn]
ronettes = [(qn + en), en, hn]
ffff = [(qn +en), en, en, en, qn]
mr = [(qn + en), (qn + en), qn,(qn + en), qn, (qn + en)]
