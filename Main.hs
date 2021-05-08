{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}
import System.Random
import Composer
import Structure
import Transform
import Euterpea
import Chord
import qualified Data.List as L
import Control.Monad.State
import Scale

main = do
  gen <- newStdGen
  let (cps,gen2) = runState (getRandoms 2 chordProgressions) gen
--  let (rs, gen3) = runState (getRandoms 2 rhythms) gen2
--  let (ps, gen4) = runState (getRandoms 2 patterns) gen3

  let inserted = toMT $ insertionsPT cps
  --let rhythmed = applyPT (rhythmsPT rs) inserted
--  let patterned = applyPT (patternsPT ps) rhythmed

  playDev 6 $ treeToMusic inserted

chordProgressions = [nico, house, dreams]
rhythms = [n, ronettes, ffff, evn 4]
patterns = [full, sad, falling, waltz, rising, sadwaltz]

-- prefix trees: ---------------------------------------------------------------

structured :: Orientation -> [[Primitive Pitch]] -> MusicTree
structured o chords = Group H $ map (toGroup o) chords

insertionsPT cs = Node (atDepth 0 [0..1]) [
              Node (atDepth 1 [0..2]) [
                Leaf (atDepth 2 [0..1]) (insert $ structured V (cs !! 0 ))
              , Leaf (atDepth 2 [2..3]) (insert $ structured V (cs !! 1))
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

-- for NOW: in OT each level the nodes have the same amount of members?

-- general patttern : genPT ::  Plan -> MusicTree -> MusicPT

data Plan = Plan { _rhythms :: [Rhythm]
                 , _patterns :: [Pattern]
                 , _chords :: [[Primitive Pitch]]
                 , _ptShape :: Shape
                 , _otShape :: Shape
        --         , _scale :: Scale
                 }

plan = Plan { _rhythms = [n, ronettes]
            , _patterns = [full, sad]
            , _chords = house
            }


data Shape = SLeaf Int | SNode [Shape]

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
genPT :: StdGen -> Plan -> MusicPT
genPT gen plan =
  let defaultPT = defaultPT (_ptShape plan)
      maxDepth = (sDepth (_otShape plan)) - 1

-- how to read Shape?





















{-
chordInsert :: Plan -> StdGen -> MusicPT
chordInsert plan =
  let c = _complexity plan
      chords = _chords plan
  in Node (getSM ) [
         Leaf (getSM) (insert $ structured V chords)
    ]


getSM :: StdGen -> Int -> (Slice -> Slice)
getSM gen d plan = atDepth d [0..(fst $ getRandom (members !! d) gen)]

--sizes gen plan = fst $ runState (getRandoms (_otDepth plan) (_members plan)) gen

-}
{-
genPT :: Plan -> MusicTree -> MusicPT
genPT plan mtree =
    let
    in

-}
