{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}
-- ^ flex instsances so we can define instance show for functions..
module Composer
( MusicOT (..)
, MusicPT(..)
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
import Data.Maybe

-- MUSIC TREE  ------------------------------------------------------------------

type MusicOT = OrientedTree (Primitive Pitch)

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions

treeToMusic :: MusicOT -> Music (Pitch, Volume)
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H trees) = line (map treeToMusic trees)
treeToMusic (Group V trees) = chord (map treeToMusic trees)

valToMusic :: MusicOT -> Music (Pitch, Volume)
valToMusic (Val (Note dur p)) = Prim ((Note dur (p, 75)))
valToMusic (Val (Rest dur)) = Prim (Rest dur)


-- GROUP TRANSFORMATIONS: ------------------------------------------------------
type TT = MusicOT -> MusicOT

toTT :: (T.Motif -> T.Motif) -> TT
toTT f = applySF f

--example gts, must be generalized:
inv = toTT $ T.invert C Major
rev  = toTT $ T.reverse
transp x = toTT $ T.transpose C Major x
strong = toTT $ T.strongCadence C Major
weak = toTT $ T.weakCadence C Major
ro = toTT . T.reorder
insert new old = new
mlSD x = toTT $ T.movelastSD C Major x
ct = toTT . T.cTrans

invTT :: MusicOT -> MusicOT
invTT = applySF $ T.invert C Major

-- SLICE TRANSFORMATIONS -------------------------------------------------------

-- some simple short hands..

atPeriods  = atDepth 0
atPhrases = atDepth 1
atMeasures = atDepth 2
atChords = atDepth 3

-- MUSIC PREFIX-TREE -----------------------------------------------------------

type MusicPT = PrefixTree (MusicOT -> MusicOT) (Slice -> Slice)

instance Show (MusicOT -> MusicOT) where
  show tt = "TT" -- cannot show a function, so just show "TT" instead

 -- Transformative Instruction:
data TI = TI { slc :: Slice, tt :: (MusicOT -> MusicOT)}

toTIs :: MusicPT -> [TI]
toTIs pt =
  let stss = getAllPaths pt
      defaultSlice = smallestDefault $ concat stss
      tts = getAllValues $ fmap ($ defaultSlice) pt
-- cant lookup functions in prefix-tree, because no instance of Eq for functions
-- so applied each slice transformation to the smallest slice of alls necessary.
  in map (toTI) $ zip stss tts

toTI :: ([Slice -> Slice], TT) -> TI
toTI (sts, ttrans) =
   TI {slc = foldl (\slc f -> f slc) (smallestDefault sts) sts, tt = ttrans}
-- left to right: ^ lower nodes can overwrite higher nodes

applyTIs :: [TI] -> MusicOT -> MusicOT
applyTIs tis tree = foldl (flip applyTI) tree tis
-- left to right: ^ functions are applied from left to right in Prefix Tree.

applyTI :: TI -> MusicOT -> MusicOT
applyTI (TI slice tt) tree = fromJust $ applyTT slice tt tree
--                              ^ Assumes applyTT works
applyPT :: MusicPT -> MusicOT -> MusicOT
applyPT pt tree = applyTIs (toTIs pt) tree

toMT :: MusicPT -> MusicOT
toMT pt = let tis = toTIs pt in applyTIs tis (makeStartingTree tis)

getSlices :: MusicPT -> [Slice]
getSlices pt = map slc $ toTIs pt

-- RHYTHMS AND PATTERNS --------------------------------------------------------

type Rhythm = [Dur] -- problem: how do we differentiate between a note and a rest?
--    ^ a rhythm, a series of durations that are looped.

evn :: Int -> [Dur] -- creates a rhythm evenly divided into x hits.
evn x = replicate x (1/fromIntegral x)

rhythm :: Rhythm -> MusicOT -> MusicOT
rhythm rm tree =
  let trees = (replicate (length rm) tree)
  in Group H $ zipWith (\dur tree -> fmap (giveDuration dur) tree) rm trees

giveDuration :: Dur -> Primitive Pitch -> Primitive Pitch
giveDuration dur (Note d p) = Note dur p

type Pattern = [[Int]]
-- ^ a pattern of scale degrees/ chord degrees. Represents a H group og V groups

-- takes a group H of group V and returns a group H of Group Vs:
pattern :: Pattern -> MusicOT -> MusicOT
pattern p (Val x) = Val x
pattern p (Group o chords) =
  Group H $ zipWith (extract) (concat $ repeat p) chords

extract :: [Int] -> MusicOT -> MusicOT
extract xs (Val x) = Val x
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
rpattern :: RPattern -> MusicOT -> MusicOT
rpattern pat tree = rpattern' pat (T.getPitches $ flatten tree)

rpattern' :: RPattern -> [Pitch] -> MusicOT
rpattern' pat pitches =
  let v (dur, ns) = Group V $ map (\n -> Val $ Note dur (pitches !! n)) ns
  -- ^ function that creates a Vertical group from one pattern-element
  in Group H $ map v pat

hDurs :: MusicOT -> Rhythm
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

makeStartingTree :: [TI] -> MusicOT
makeStartingTree tis =
  let slices        = map slc tis
      treeStructure = foldr addSlice TLeaf slices
  in  toDefaultOrientedTree treeStructure

toDefaultOrientedTree :: TreeShape -> MusicOT
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
