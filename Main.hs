import System.Random
import Composer
import Structure
import Transform
import Euterpea
import Chord
import qualified Data.List as L
import Control.Monad.State

main = do
  gen <- newStdGen
  let (cps,gen2) = runState (getRandoms 1 chordProgressions) gen
  let (rs, gen3) = runState (getRandoms 2 rhythms) gen2
  let (ps, gen4) = runState (getRandoms 2 patterns) gen3

  let inserted = toMT $ insertionsPT cps
  let rhythmed = applyPT (rhythmsPT rs) inserted
  let patterned = applyPT (patternsPT ps) rhythmed

  playDev 6 $ treeToMusic patterned

chordProgressions = [nico, house, dreams]
rhythms = [n, ronettes, ffff, evn 4]
patterns = [full, sad, falling, waltz, rising, sadwaltz]

-- prefix trees: ---------------------------------------------------------------

-- initially create a datastructure [Int] that contains the amount of members
-- for each level in the MT.

structured :: Orientation -> [[Primitive Pitch]] -> MusicTree
structured o chords = Group H $ map (toGroup o) chords

insertionsPT cs = Node (atPhrases [0..3]) [
              Leaf (atPeriods [0,1]) (insert $ structured V (head cs))
            ]

rhythmsPT rs  = Node (atPeriods [0,1] . atMeasures [0,1]) [
                        Leaf (atPhrases [0,1]) (rhythm (rs !! 0))
                    ,   Leaf (atPhrases [2,3]) (rhythm (rs !! 1))
                    ]

patternsPT' ps = Leaf (atPeriods [1]) (transp 2 . inv )

patternsPT ps = Node (atMeasures [0,1]) [
                  Node (atPhrases [0,1]) [
                    Leaf (atPeriods[0,1]) (pattern (ps !! 0))
                 ]
                , Node (atPhrases [2,3]) [
                  Leaf (atPeriods[0,1]) (pattern (ps !! 1))
               ,  Leaf (atPeriods[1]) (pattern (ps !! 0))
               ]
            ]

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

-- general patttern : genPT ::  Plan -> MusicTree -> MusicPT

data Plan = Plan { size :: [Int], rhythms :: [Rhythm], patterns :: [Pattern]}

genPT :: MusicTree -> MusicPT

genPT
