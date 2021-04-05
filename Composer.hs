module Composer
( MusicTree (..)
, treeToMusic
, period
, inv
, rev
, transp
, giveR
, strong
, weak
, ro
)
where

import Scale
import Structure
import Euterpea
import qualified Transform as T

-- MUSIC TREES ------------------------------------------------------------------

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

type MusicPT = PrefixTree GT (Slice -> Slice)

-- SLICE CONSTRUCTION ----------------------------------------------------------

-- slice construction: allows the composition of (Slice -> Slice)
-- examples that apply to "testTree": (need to be generalized)
-- should they add? i.e. atVoices[0,1] . atVoices[2] = atVoices [0,1,2]?
-- right now atVoices[0,1] . atVoices[2] = atVoices [0,1]

atVoices, atPeriods, atMotifs :: [Int] -> Slice -> Slice
atVoices selection [a, b, c] = [Some selection, b, c]
atPeriods selection [a, b, c] = [a, Some selection, c]
atMotifs selection [a, b, c] = [a, b, Some selection ]

-- GROUP TRANSFORMATIONS: ------------------------------------------------------
type GT = MusicTree -> MusicTree

toGT :: (T.Motif -> T.Motif) -> GT
toGT f group@(Group o _) = toGroup o $ f $ fromGroup group

inv = toGT $ T.invert C Major
rev  = toGT $ T.reverse
transp x = toGT $ T.transpose C Major x
givePs group = toGT $ T.givePitches (fromGroup group)
giveR group = toGT $ T.giveRhythm (fromGroup group)
strong = toGT $ T.strongCadence C Major
weak = toGT $ T.weakCadence C Major
ro = toGT . T.reorder
insert new old = new
ext = toGT . T.extend
mlSD x = toGT $ T.movelastSD C Major x

-- TRANSFORMATIVE INSTRUCTIONS -------------------------------------------------

data TI = TI { slc :: Slice, gt :: GT} -- Transformative Instruction

applyTIs :: [TI] -> MusicTree -> MusicTree
applyTIs instructions startingTree =
  foldl (flip applyTI) startingTree instructions

applyTI :: TI -> MusicTree -> MusicTree
applyTI (TI slice gt) tree = applyGT slice gt tree

tis2Tree :: [TI] -> MusicTree
tis2Tree instructions = applyTIs instructions (makeStartingTree instructions)

toTI :: Slice -> ([Slice -> Slice], GT) -> TI
toTI levels (strans, gtrans) =
   TI {slc = foldl(\acc f -> f acc) levels strans, gt = gtrans }

pt2Tree :: MusicPT -> MusicTree
pt2Tree pt =
  let levels = [All, All, All]
      values = getAllValues $ fmap ($ levels) pt
      paths = getAllPaths pt
  in tis2Tree $ map (toTI levels) $ zip paths values


-- TESTING ZONE: ---------------------------------------------------------------

p tree = playDevS 6 $ tempo 0.80 $ (treeToMusic tree) --quick play

motif, motif2, motif3 :: T.Motif
motif2 = [Note qn (C,4), Note qn (C,4), Note qn (B,3), Note qn (E,4)]
motif3 = [Note sn (A,4), Note sn (A,4), Note qn (B,4), Note qn (C,4)]

base m = (Group H $ map (toGroup H) $  replicate 4 m) :: MusicTree
period = applyGT [Some[1]] (weak. transp (-2)) .
  applyGT [Some[3]] (strong . inv) . applyGT [Some[1,3]] (giveR m3) . base

m1, m2, m3, m4 :: MusicTree
m1 = toGroup H [Note hn (C,3), Note qn (E,3), Note qn (F,3)]
m2 = toGroup H [Note qn (C,2), Note hn (D,2), Note qn (E,2)]
m4 = toGroup H [Note qn (C,4), Note en (D,4), Note en (B,4)]


motif = [Note qn (C,4), Rest en, Note en (D,4), Note qn (E,4), Note qn (B,4)]
m3 = toGroup H [Note qn (C,4), Note qn (D,4), Note hn (B,4)]

cvMotif :: MusicTree
cvMotif = toGroup H [Note en (C,4), Note en (C,4), Note en (C,4), Note en (C,4)]
cvMotif2 = toGroup H [Rest sn, Note en (C,4), Note en (C,4), Note en (C,4), Note sn (C,4)]
cMajor = toGroup V [Note hn (C,4), Note hn (G,3)]
-- first attempt at "cellevevet"
cv :: MusicPT
cv = Node (atPeriods [0,1,2,3,4,5,6,7]) [
         Node (atVoices [0] ) [ --
            Leaf (atMotifs [0,1,2,3,4,5,6,7]) (insert $ transp (-7) cvMotif)
         ,  Leaf (atMotifs [2,3]) (transp (-2))
         ,  Leaf (atMotifs [4,5]) (transp (-4))
         ,  Leaf (atMotifs [1,3]) (mlSD (-1))
         ,  Leaf (atMotifs [6]) (transp 1)
         ,  Leaf (atMotifs [7]) (transp(-3))
        ]
     ,  Node (atVoices [1]) [
          Leaf (atMotifs [0,1,2,3,4,5,6,7]) (insert $ cvMotif2)
        , Leaf (atPeriods [3,5,7] . atMotifs [7]) (transp (-1))
        ]
     ,  Node (atPeriods [4,5]) [
          Node (atVoices [0,1]) [
          Leaf (atMotifs [0,1,2,3,4,5,6,7]) (transp (-3))
          ]
        , Node (atVoices [1]) [
            Leaf (atMotifs [3,7]) (transp (-1))
          , Leaf (atMotifs [2]) (transp(1))
          ]
       ]
     , Leaf (atPeriods [8] . atMotifs[0]) (insert $ cMajor)
     ]

-- need to combine multiplegroups and act on them like 1 !!

{-
testPT :: MusicPT
testPT = Node (atPeriods [0,1,2,3]) [
            Node (atMotifs [0,1,2,3])[
              Leaf (atPeriods [0,1]) (insert $ toGroup H motif)
            , Leaf (atPeriods [2,3]) (insert $ toGroup H motif2)
            ]
         ,  Leaf (atMotifs [1,3]) (ext 1 . giveR m3 . inv)
         ,  Node (atPeriods [1]) [
              Leaf (atMotifs [0,1,2,3]) (transp 2)
            ]
         ,  Leaf (atMotifs [1]) weak
         ,  Leaf (atMotifs [3]) strong
         ]
-}
testTIs = [ TI [All, All] (insert $ toGroup H motif),
            TI [Some[1,3], Some[1,3]] inv,
            TI [All, Some[1]] (ext 1 . giveR m3 . weak . transp (-2)),
            TI [All, Some[3]] (ext 1. giveR m3 .strong),
            TI [Some [2,3], All] (transp 3),
            TI [Some [4,5], All] (inv . rev . transp (-2))
           ]

lol = [TI [Some[0,1,2,3]] (insert para),
       TI [All, Some[1], All] (transp (-14)),
       TI [All, Some[1], Some[1,3]] (rev),
       TI [Some[2,3], Some[0], All] (transp 2),
       TI [All, All, Some[3]] (transp (-1))
       ]

para = Group V [chords, applyGT [All] (giveR m4 .toGroup H . fromGroup . transp 7) chords]

startingTree = base motif

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
  go $ [Group V, Group H, Group H] ++ repeat (Group H) -- left is top
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
