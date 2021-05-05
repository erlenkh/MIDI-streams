module Composer
( MusicTree (..)
, treeToMusic
, inv
, rev
, transp
, strong
, weak
, ro
, atPhrases
, atMeasures
, atPeriods
, evn
, insert
, rhythm
, pattern
, Pattern(..)
, Rhythm(..)
, toMT
, applyPT
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

atPeriods  = atDepth 0
atPhrases = atDepth 1
atMeasures = atDepth 2
atChords = atDepth 3

-- MUSIC PREFIX-TREE -----------------------------------------------------------

type MusicPT = PrefixTree (MusicTree -> MusicTree) (Slice -> Slice)

 -- Transformative Instruction:
data TI = TI { slc :: Slice, gt :: (MusicTree -> MusicTree)}

toTIs :: MusicPT -> [TI]
toTIs pt =
  let stss = getAllPaths pt
      defaultSlice = smallestDefault $ concat stss
      gts = getAllValues $ fmap ($ defaultSlice) pt
-- cant lookup functions in prefix-tree, because no instance of Eq for functions
-- so applied each slice transformation to the smallest slice of alls necessary.
  in map (toTI) $ zip stss gts

toTI :: ([Slice -> Slice], GT) -> TI
toTI (sts, gtrans) =
   TI {slc = foldl (\slc f -> f slc) (smallestDefault sts) sts, gt = gtrans}
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

-- RHYTHMS AND PATTERNS --------------------------------------------------------

type Rhythm = [Dur] -- problem: how do we differentiate between a note and a rest?
--    ^ a rhythm, a series of durations that are looped.

evn :: Int -> [Dur] -- creates a rhythm evenly divided into x hits.
evn x = replicate x (1/fromIntegral x)

rhythm :: Rhythm -> MusicTree -> MusicTree
rhythm rm tree =
  let trees = (replicate (length rm) tree)
  in Group H $ zipWith (\dur tree -> fmap (giveDuration dur) tree) rm trees

giveDuration :: Dur -> Primitive Pitch -> Primitive Pitch
giveDuration dur (Note d p) = Note dur p

type Pattern = [[Int]]
-- ^ a pattern of scale degrees/ chord degrees. Represents a H group og V groups

-- takes a group H of group V and returns a group H of Group Vs:
pattern :: Pattern -> MusicTree -> MusicTree
pattern p (Group o chords) =
  Group H $ zipWith (extract) (concat $ repeat p) chords

extract :: [Int] -> MusicTree -> MusicTree
extract xs (Group o ns) =
  let sel = L.sort xs
  in if length ns > maximum sel then Group V $ map (ns !!) sel
     else extract (unique $ init sel ++ [length ns - 1]) (Group o ns)

unique = S.toList . S.fromList

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

hDurs :: MusicTree -> Rhythm
hDurs tree = fmap T.getDur $ flatten tree

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
