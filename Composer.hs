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
import qualified Data.Set as S

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
atChords = atDepth 3

--crashes if lvl >= length slice (might fix with Maybe)
atLevel :: Int -> [Int] -> (Slice -> Slice)
atLevel lvl selection slice =
  let (first, second) = splitAt lvl (reverse slice)
  in reverse $ first ++ [Some selection] ++ tail second

-- selection should be a slice!
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
toTI (sts, gtrans) =
   TI {slc = foldl (\slc f -> f slc) (smallestAlls sts) sts, gt = gtrans}
-- left to right: ^ lower nodes can overwrite higher nodes

applyTIs :: [TI] -> MusicTree -> MusicTree
applyTIs tis tree = foldl (flip applyTI) tree tis

applyTI :: TI -> MusicTree -> MusicTree
applyTI (TI slice gt) tree = applyGT slice gt tree

applyPT :: MusicPT -> MusicTree -> MusicTree
applyPT pt tree = applyTIs (toTIs pt) tree

toMT :: MusicPT -> MusicTree
toMT pt = let tis = toTIs pt in applyTIs tis (makeStartingTree tis)

getSlices :: MusicPT -> [Slice]
getSlices pt = map slc $ toTIs pt

-- TESTING GENERATING PREFIX TREES: --------------------------------------------

mkc p o m ints dur = map (\p -> Note dur p) $ pitches $ getChord (p,o) m ints

nico = [mkc C 3 Major [2,4,6] wn, mkc F 3 Major [2,4,6] wn]
house = [mkc A 3 Minor [2,4,6] wn, mkc E 3 Minor [2,4,6] wn]
dreams = [mkc C 3 Major [2,4,6] wn, mkc A 2 Minor [2,4,6] wn]

insertions = Node (atPhrases [0..3]) [
              Leaf (atPeriods [0])(insert $ structured V house)
            , Leaf (atPeriods [1])(insert $ structured V nico)
            ]

inserted = toMT insertions

patterns tree = Node (atPhrases [0..3]) [
                        Leaf (atMeasures [0]) (rhythm (n))
                     ,  Leaf (atMeasures [1]) (rhythm (evn 32))
            --        ,  Leaf (atDepth 2 [0,1]) (transp 7)
                  ]

patterned = applyPT (patterns inserted) inserted

sections = Node (atMeasures [0,1]) [
              Node (atPhrases [0,1]) [
                Leaf (atPeriods[0]) (pattern falling)
              , Leaf (atPeriods[1]) (pattern rising)
              ]
          ,  Leaf (atPeriods [0,1] . atPhrases [2,3]) (pattern full)
            ]

sectioned = applyPT (sections) patterned

structured :: Orientation -> [[Primitive Pitch]] -> MusicTree
structured o chords = Group H $ map (toGroup o) chords

type Rhythm = [Dur] -- problem: how do we differentiate between a note and a rest?
--    ^ a rhythm, a series of durations that are looped.

evn :: Int -> [Dur] -- creates a rhythm evenly divided into x hits.
evn x = replicate x (1/fromIntegral x)

-- basic rhythms:
n = [(qn + en), (qn + en), qn]
ronettes = [(qn + en), en, hn]
ffff = [(qn +en), en, en, en, qn]
mr = [(qn + en), (qn + en), qn,(qn + en), qn, (qn + en)]

type Pattern = [[Int]]
-- ^ a pattern of scale degrees/ chord degrees. Represents a H group og V groups

-- basic patterns:
full, falling, waltz, rising :: Pattern
full = [[0,1,2]]
falling = [[2], [1], [0]]
waltz = [[0], [1,2], [1,2]]
rising = reverse falling

type RPattern = [(Dur, [Int])]
-- ^ Rhythmic pattern: what notes should be played for each duration.

rp :: Rhythm -> Pattern -> RPattern
rp r p = zip r (concat $ repeat p)

-- takes in a pattern and a musicTree, and gives out a musictree with the
-- pitches from the OG tree in the form of the pattern:
rpattern :: RPattern -> MusicTree -> MusicTree
rpattern pat tree = rpattern' pat (T.getPitches $ flatten tree)

rpattern' :: RPattern -> [Pitch] -> MusicTree
rpattern' pat pitches =
  let v (dur, ns) = Group V $ map (\n -> Val $ Note dur (pitches !! n)) ns
  -- ^ function that creates a Vertical group from one pattern-element
  in Group H $ map v pat

rhythm :: Rhythm -> MusicTree -> MusicTree
rhythm rm tree =
  let trees = (replicate (length rm) tree)
  in Group H $ zipWith (\dur tree -> fmap (giveDuration dur) tree) rm trees

-- takes a group H of group V and returns a group H of Group Vs

pattern :: Pattern -> MusicTree -> MusicTree
pattern p (Group o chords) =
  Group H $ zipWith (extract) (concat $ repeat p) chords

extract :: [Int] -> MusicTree -> MusicTree
extract xs (Group o ns) =
  let sel = L.sort xs
  in if length ns > maximum sel then Group V $ map (ns !!) sel
     else extract (unique $ init sel ++ [length ns - 1]) (Group o ns)

unique = S.toList . S.fromList

hDurs :: MusicTree -> Rhythm
hDurs tree = fmap T.getDur $ flatten tree

giveDuration :: Dur -> Primitive Pitch -> Primitive Pitch
giveDuration dur (Note d p) = Note dur p

-- TESTING ZONE: ---------------------------------------------------------------

play2 t1 t2 = playDevS 6 $ treeToMusic t1 :=: treeToMusic t2

p tm tree = playDevS 6 $ tempo tm $ (treeToMusic tree) --quick play


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
