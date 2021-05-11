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

structured :: Orientation -> [[Primitive Pitch]] -> MusicOT
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

-- general patttern : genPT ::  Plan -> MusicOT -> MusicPT

data Plan = Plan { _rhythms :: [Rhythm]
                 , _patterns :: [Pattern]
                 , _chords :: [[Primitive Pitch]]
                 , _ptShape :: Shape
  --               , _otShape :: Shape
        --         , _scale :: Scale
                 }

plan = Plan { _rhythms = [n, ronettes]
            , _patterns = [full, sad]
            , _chords = house
            , _ptShape = shape
            }


data Shape = SLeaf Int | SNode [Shape] deriving Show -- problematic?
-- what if a node contains 2 Vals and 1 Group! This thing is invalid!

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
genPT :: StdGen -> Plan -> (OrientedTree a) -> MusicPT
genPT gen plan oTree =
  let dpt = defaultPT (_ptShape plan)
      sliceMods = genSliceMods gen dpt oTree
  in elevatePT sliceMods dpt


genSliceMods gen pt ot =
  let (depths, gen2) = randomDepths gen (keys pt) ot
      (widths, gen3) = randomWidths gen depths ot
  in zipWith (\d w -> atDepth d [0..w]) depths widths


randomDepths :: StdGen -> Int -> OrientedTree a -> ([Int], StdGen)
randomDepths gen n ot = runState (getRandoms n (depthRange ot)) gen

randomWidths :: StdGen -> [Int] -> OrientedTree a -> ([Int], StdGen)
randomWidths gen depths ot =
  runState (sequence $ map (randomSt . widthRange ot) depths) gen



testMT :: OrientedTree Char
testMT =     Group H [
                Group V [
                  Val 'C',
                  Val 'A',
                  Val 'T'
                ],
                Group V [
                  Val 'D',
                  Val 'O',
                  Val 'G'
                ],
                Group H [
                  Group H [
                    Val 'K',
                    Val 'I',
                    Val 'L'
                  ],
                  Group H [
                    Val 'L',
                    Val 'E',
                    Val 'R'
                  ]
                ]
              ]

testOT :: OrientedTree Int
testOT = Group H [Group V [Val 1, Val 2, Val 3], Group V [Val 4, Val 5]]














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
genPT :: Plan -> MusicOT -> MusicPT
genPT plan mtree =
    let
    in

-}
