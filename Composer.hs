module Composer
( MusicTree (..)
, treeToMusic
, inv
, rev
, transp
, strong
, weak
, ro
)
where

import Scale
import Structure
import Euterpea
import qualified Transform as T
import Chord
import qualified Data.List as L

-- MUSIC TREE  ------------------------------------------------------------------

type MusicTree = OrientedTree (Primitive Pitch)

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions

treeToMusic :: MusicTree -> Music (Pitch, Volume)
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H trees) = line (map treeToMusic trees)
treeToMusic (Group V trees) = chord (map treeToMusic trees)

valToMusic :: MusicTree -> Music (Pitch, Volume)
valToMusic (Val (Note dur p)) = Prim ((Note dur (p, 75)))
valToMusic (Val (Rest dur)) = Prim (Rest dur)


-- GROUP TRANSFORMATIONS: ------------------------------------------------------
type GT = MusicTree -> MusicTree

toGT :: (T.Motif -> T.Motif) -> GT
toGT f = applySF f

--example gts, must be generalized:
inv = toGT $ T.invert C Major
rev  = toGT $ T.reverse
transp x = toGT $ T.transpose C Major x
strong = toGT $ T.strongCadence C Major
weak = toGT $ T.weakCadence C Major
ro = toGT . T.reorder
insert new old = new
mlSD x = toGT $ T.movelastSD C Major x
ct = toGT . T.cTrans

invGT :: MusicTree -> MusicTree
invGT = applySF $ T.invert C Major


-- SLICE TRANSFORMATIONS -------------------------------------------------------

-- slice transformations: construction of slices by composition STs

-- should they add? i.e. atVoices[0,1] . atVoices[2] = atVoices [0,1,2]?
-- right now atVoices[0,1] . atVoices[2] = atVoices [0,1]

type ST = (Slice -> Slice)

atPeriods  = atDepth 0
atPhrases = atDepth 1
atMeasures = atDepth 2

--crashes if lvl >= length slice (might fix with Maybe)
atLevel :: Int -> [Int] -> (Slice -> Slice)
atLevel lvl selection slice =
  let (first, second) = splitAt lvl (reverse slice)
  in reverse $ first ++ [Some selection] ++ tail second

-- ideally this should return a maybe but it is a lot of work just for idealism:
atDepth :: Int -> [Int] -> (Slice -> Slice) -- is used by partial application
atDepth lvl selection slice =
  let (first, second) = splitAt lvl slice
  in first ++ [Some selection] ++ tail second

smallestAlls :: [Slice -> Slice] -> Slice
smallestAlls sts = replicate ((getMaxDepth sts) + 1) All

getMaxDepth :: [Slice -> Slice] -> Int
getMaxDepth sts = maximum $ map getDepth sts

-- a piece cannot have more that 666 hierarchical levels, should be generalized
getDepth :: (Slice -> Slice) -> Int
getDepth sTrans = maximum $ L.findIndices (isSome) $ sTrans $ replicate (666) All

isSome (Some xs) = True
isSome _  = False

-- MUSIC PREFIX-TREE -----------------------------------------------------------

type MusicPT = PrefixTree (MusicTree -> MusicTree) (Slice -> Slice)

 -- Transformative Instruction:
data TI = TI { slc :: Slice, gt :: (MusicTree -> MusicTree)}

toTIs :: MusicPT -> [TI]
toTIs pt =
  let stss = getAllPaths pt
      alls = smallestAlls $ concat stss
      gts = getAllValues $ fmap ($ alls) pt
-- cant lookup functions in prefix-tree, because no instance of Eq for functions
-- so applied each slice transformation to the smallest slice of alls necessary.
  in map (toTI) $ zip stss gts

toTI :: ([Slice -> Slice], GT) -> TI
toTI (sts, gtrans) = TI {slc = foldr ($) (smallestAlls sts) sts, gt = gtrans}

applyTIs :: [TI] -> MusicTree -> MusicTree
applyTIs tis tree = foldl (flip applyTI) tree tis

applyTI :: TI -> MusicTree -> MusicTree
applyTI (TI slice gt) tree = applyGT slice gt tree

toMT :: MusicPT -> MusicTree
toMT pt = let tis = toTIs pt in applyTIs tis (makeStartingTree tis)

getSlices :: MusicPT -> [Slice]
getSlices pt = map slc $ toTIs pt

-- TESTING GENERATING PREFIX TREES: --------------------------------------------

--mkc p o m dur = map (\p -> Note dur p) $ pitches $ getTriad (p,o) m
mkc p o m ints dur = map (\p -> Note dur p) $ pitches $ getChord (p,o) m ints

nico = [mkc C 3 Major [2,4,6] wn, mkc F 3 Major [2,4,6] wn]
house = [mkc A 3 Minor [2,4,6] wn, mkc E 3 Minor [2,4,6] wn]
dreams = [mkc C 3 Major [2,4,6] wn, mkc A 2 Minor [2,4,6] wn]
pain = [mkc C 2 Major [2,4,6] wn, mkc A 2 Minor [2,4,6] wn]
spanish = [mkc E 3 Major [2,4,6] wn, mkc F 3 Major [2,4,6] wn]

-- need sequential insertion / other way to generate prefix tree:
pt pat1 pat2 c = Node (atDepth 0 [0,1])[
              Leaf (atDepth 2 [0]) (insert $ toGroup V $ c !! 0)
          ,   Leaf (atDepth 2 [1]) (insert $ toGroup V $ c !! 1)
          ,   Node (atDepth 2 [0,1])[
                Leaf (atDepth 1 [0,1]) ( pattern pat1)
              ]
          ,   Leaf ((atDepth 2 [1]) . (atDepth 1 [1])) (pattern pat2)
         ]

type Rhythm = [Dur] -- problem: how do we differentiate between a note and a rest?
--    ^ a rhythm, a series of durations that are looped.

evn :: Int -> [Dur] -- creates a rhythm evenly divided into x hits.
evn x = replicate x (1/fromIntegral x)

-- example rhythms:
n = [(qn + en), (qn + en), qn]
fifties = [(qn +en), en, en, en, qn]

type Pattern = [(Dur, [Int])]
-- ^ what notes should be played for each duration

-- examples:
p0, p1, p11, p2, p3, p4, pn :: Pattern
pn = zip (evn 1) (concat $ repeat [[0,1,2]])
p0 = zip (n) (concat $ repeat [[0,1,2]])
p1 = zip (evn 6) (concat $ repeat [[0],[1,2],[1,2]])
p11 = zip (evn 6) (concat $ repeat [[0],[1,2,3],[1,2,3]])
p2 = zip (evn 8) (concat $ repeat [[0], [2,3]])
p3 = zip (evn 32) (concat $ repeat [[0], [1], [3], [2]])
p4 = zip (fifties) (concat $ repeat [[0,1,2]])

-- takes in a pattern and a musicTree, and gives out a musictree with the
-- pitches from the OG tree in the form of the pattern.

pattern :: Pattern -> MusicTree -> MusicTree
pattern pat tree = pattern' pat (T.getPitches $ flatten tree)

pattern' :: Pattern -> [Pitch] -> MusicTree
pattern' pat pitches =
  let v (dur, ns) = Group V $ map (\n -> Val $ Note dur (pitches !! n)) ns
  -- ^ function that creates a Vertical group from one pattern-element
  in Group H $ map v pat

rhythm :: Rhythm -> MusicTree -> MusicTree
rhythm rm tree =
  let newtree = (replicate (length rm) tree)
  in Group H $ zipWith (\dur tree -> fmap (giveDuration dur) tree) rm newtree

hDurs :: MusicTree -> Rhythm
hDurs tree = fmap T.getDur $ flatten tree

giveDuration :: Dur -> Primitive Pitch -> Primitive Pitch
giveDuration dur (Note d p) = Note dur p

-- TESTING ZONE: ---------------------------------------------------------------

p tm tree = playDevS 6 $ tempo tm $ (treeToMusic tree) --quick play

mkChord pitch mode dur = map (\p -> Note dur p) $ pitches $ getTriad pitch mode

mc p o m = insert $ toGroup V $ mkChord (p,o) m hn

cv      = Node (atPeriods [0,1,2,3,4,5,6,7]) [
              Node (atPeriods [0,1,2,6,7]) [
                Node (atMeasures [0,1]) [
                    Leaf (atPhrases [0]) (mc C 3 Major)
                ,   Leaf (atPhrases [1]) (mc A 2 Minor)
                ,   Leaf (atPhrases [2]) (mc F 2 Major)
                ,   Node (atPhrases [3]) [
                        Leaf (atMeasures [0]) (mc D 3 Minor)
                    ,   Leaf (atMeasures [1]) (mc G 2 Major)
                    ]
                ]
                , Leaf (atMeasures [0,1]) (toCV1)
                , Leaf (atPhrases [0,1] . atMeasures [1] . atDepth 3 [0]) (mlSD (-1))
                , Leaf (atPeriods [2,7] . atPhrases [3] . atMeasures [1] . atDepth 3 [1]) (transp (-1))
            ]
            , Node (atPeriods [3,4,5,8,9,10]) [
                Node (atMeasures [0,1]) [
                    Leaf (atPhrases [0]) (mc F 2 Major)
                ,   Leaf (atPhrases [2]) (mc A 2 Minor)
                ,   Leaf (atPhrases [3]) (mc A 2 Major)
                ,   Node (atPhrases [1]) [
                        Leaf (atMeasures [0]) (insert $ toGroup V [Note hn (E,2), Note hn (A,2), Note hn ((B,2))])
                    ,   Leaf (atMeasures [1]) (mc E 2 Major)
                    ]
                ]
              , Leaf (atMeasures [0,1]) (toCV2)
              ]
          ]

toCV' :: [Pitch] -> [Pitch] -> MusicTree
toCV' n1 n2 =
  let f = L.intersperse (Val $ Rest sn) . concat . replicate 4
      g1 = Group H $ f [toGroup V $ map (\p -> Note sn p) n1] ++ [Val $ Rest sn]
      g2 = Group H $ (Val $ Rest sn) : f [toGroup V $ map (\p -> Note sn p) n2]
  in Group V [g1, g2]

toCV1 tree =
  let notes = T.getPitches $ flatten tree
  in toCV' [head notes] [(C,4)]

toCV2 tree =
  let notes = T.getPitches $ flatten tree
  in toCV' [head notes] (tail notes)

chords :: OrientedTree (Primitive Pitch)
chords =
              Group H [
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ]
                ]


melody :: MusicTree
melody = Group H [
              Group H [
                  Group H [
                    Val (Note hn (C,4)),
                    Val (Note hn (E,4)),
                    Val (Note hn (G,4))
                    ],
                  Group H [
                    Val (Note hn (C,4)),
                    Val (Note hn (E,4)),
                    Val (Note hn (G,4))
                    ]
              ]
          ,   Group H [
                    Group H [
                      Val (Note hn (C,4)),
                      Val (Note hn (E,4)),
                      Val (Note hn (G,4))
                      ],
                    Group H [
                      Val (Note hn (D,4)),
                      Val (Note hn (E,4)),
                      Val (Note hn (G,4))
                      ]
                ]
          ]

-- MODIFIED YAN HAN: -----------------------------------------------------------

-- Used only to infer a skeletal 'OrientedTree' from a musical prefix tree.
data TreeShape =
  TAll TreeShape
  | TSome [TreeShape]
  | TLeaf
  deriving Show

makeStartingTree :: [TI] -> MusicTree
makeStartingTree tis =
  let slices        = map slc tis
      treeStructure = foldr addSlice TLeaf slices
  in  toDefaultOrientedTree treeStructure

toDefaultOrientedTree :: TreeShape -> MusicTree
toDefaultOrientedTree =
  go $ repeat (Group H) -- left is top
 where
  go (c : cs) TLeaf      = Group H [Val $ Rest sn]
  go (c : cs) (TAll  t ) = c [go cs t]
  go (c : cs) (TSome ts) = c . map (go cs) $ ts

extendList :: Int -> a -> [a] -> [a]
extendList n e xs | n <= length xs = xs
                  | otherwise      = xs ++ replicate (n - length xs) e

mapChoice :: [Int] -> (a -> a) -> [a] -> [a]
mapChoice idxs f as =
  zipWith (\a idx -> if idx `elem` idxs then f a else a) as [0 ..]

addSlice :: Slice -> TreeShape -> TreeShape
addSlice []         t          = t
addSlice (All : xs) TLeaf      = TAll (addSlice xs TLeaf)
addSlice (All : xs) (TAll  t ) = TAll (addSlice xs t)
addSlice (All : xs) (TSome ts) = TSome (map (addSlice xs) ts)
addSlice (Some is : xs) TLeaf =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) TLeaf))
addSlice (Some is : xs) (TAll t) =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) t))
addSlice (Some is : xs) (TSome ts) =
  TSome (mapChoice is (addSlice xs) (extendList (maximum is + 1) TLeaf ts))
