{-# LANGUAGE FlexibleInstances, DeriveFunctor, DeriveTraversable #-}
module Transform(
  getDur
, reorder
, Order (..)
, getPitches
, evn
, rhythm
, rhythm'
, pattern
, Pattern(..)
, Rhythm(..)
, Transform.insert
, inv
, rev
, transp
, strong
, weak
, ro
) where

import Scale
import MusicTrees
import Trees

import Euterpea
import Data.List
import Data.Maybe
import qualified Data.Set as S

-- TREE TRANSFORMATIONS: ------------------------------------------------------
type TT = MusicOT -> MusicOT

instance Show (MusicOT -> MusicOT) where
  show tt = "TT" -- cannot show a function, so just show "TT" instead

toTT :: (Motif -> Motif) -> TT
toTT f = applySF f

cMajor = Scale C Major

--example gts, must be generalized:
inv = toTT $ Transform.invert cMajor
rev  = toTT $ Transform.reverse
transp x = toTT $ Transform.transpose cMajor x
strong = toTT $ strongCadence cMajor
weak = toTT $ weakCadence cMajor
ro = toTT . reorder
insert new old = new
mlSD x = toTT $ movelastSD cMajor x
ct = toTT . cTrans

  -- rhythms and patterns  -----------------------------------------------------

type Rhythm = [Dur] -- problem: how do we differentiate between a note and a rest?
--    ^ a rhythm, a series of durations that are looped.

evn :: Int -> [Dur] -- creates a rhythm evenly divided into x hits.
evn x = replicate x (1/fromIntegral x)

rhythm :: Rhythm -> MusicOT -> MusicOT
rhythm rm tree =
  let ts = (replicate (length rm) tree)
  in Group H $ zipWith (\dur t -> fmap (giveDuration dur) t) rm ts

rhythm' :: Rhythm -> MusicOT -> MusicOT
rhythm' rm tree =
  let td = totDur tree
      trees = (replicate (length rm) tree)
  in Group H $ zipWith (\dur tree -> fmap (giveDuration (dur/td)) tree) rm trees

giveDuration :: Dur -> Primitive Pitch -> Primitive Pitch
giveDuration dur (Note d p) = Note dur p

type Pattern = [[Int]]
-- ^ a pattern of scale degrees/ chord degrees. Represents a H group og V groups

-- takes a group H of group V and returns a group H of Group Vs:
pattern :: Pattern -> MusicOT -> MusicOT
pattern p (Val x) = Val x
-- ^ Should never happen, and makes no sense. included to avoid crash for now.
pattern p (Group o chords) =
  Group H $ zipWith (extract) (concat $ repeat p) chords

extract :: [Int] -> MusicOT -> MusicOT
extract xs (Val x) = Val x
-- ^ Should never happen, and makes no sense. included to avoid crash for now.
extract xs (Group o ns) =
  let sel = sort xs
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
rpattern pat tree = rpattern' pat (getPitches $ flatten tree)

rpattern' :: RPattern -> [Pitch] -> MusicOT
rpattern' pat pitches =
  let v (dur, ns) = Group V $ map (\n -> Val $ Note dur (pitches !! n)) ns
  -- ^ function that creates a Vertical group from one pattern-element
  in Group H $ map v pat

hDurs :: MusicOT -> Rhythm
hDurs tree = fmap getDur $ flatten tree

totDur :: MusicOT -> Dur
totDur (Group H trees) = sum $ map totDur trees
totDur (Group V trees) = maximum $ map totDur trees
totDur (Val x) = getDur x


-- MOTIF TRANSFORMATION IN SCALE CONTEXT --------------------------------------

--TODO: Clean up, rename to comply with standards, and handle Maybe better...

type Motif = [Primitive Pitch]

-- transpose: Transposition of Motif by a given amount of Scale Degrees
transpose :: Scale -> ScaleDeg -> Motif -> Motif
transpose scale deg motif = map (primTrans scale deg) motif

-- reverse: Reversion of pitch sequence but not durations or rests
reverse :: Motif -> Motif
reverse motif =
  let motifP = getPitches motif -- removes rests
      revMotifP = Data.List.reverse motifP
  in replacePitches revMotifP motif

-- fullReverse: Full reversion of motif
fullReverse :: Motif -> Motif
fullReverse motif = Data.List.reverse motif

-- invert: Diatonic inversion of Motif around first note, preserving Rests
invert :: Scale -> Motif -> Motif
invert scale motif =
  let motifP = getPitches motif -- removes rests
      motifSD = map (toSD scale) $ map (absPitch) motifP
      invMotifSD = invertSD motifSD
      invMotifP = map (pitch . toAP scale) $ invMotifSD
  in  replacePitches invMotifP motif  -- replace inv pitches in motif

-- gives pitches of a giver motif to a taker motif
givePitches :: Motif -> Motif -> Motif
givePitches giver taker = replacePitches (getPitches giver) taker

-- gives the rhythm (duration and rest info) of a giver motif to a taker motif
-- returns a new motif with the pitches of the taker and the rhythm of the giver
giveRhythm :: Motif -> Motif -> Motif
giveRhythm giver taker = replacePitches (getPitches taker) giver

strongCadence :: Scale -> Motif -> Motif
strongCadence scale motif = changelastSD scale 0 motif

weakCadence :: Scale -> Motif -> Motif
weakCadence scale motif = changelastSD scale 4 motif

replaceDurations :: [Dur] -> Motif -> Motif
replaceDurations durs motif = zipWith replaceDuration durs motif

-- replacePitches: takes a sequence of Pitches and a Motif and changes the
-- Pitches while preserving the Rests and durations.
replacePitches :: [Pitch] -> Motif -> Motif
replacePitches _ [] = []
replacePitches  pitches motif  =
  let first = takeWhile (notRest) motif
      second = dropWhile (notRest) motif
      splitPs = splitAt (length first) pitches
      newPrimPs = zipWith replacePitch  (fst splitPs) first
      rest = take 1 second
  in newPrimPs ++ rest ++ replacePitches  (snd splitPs) (drop 1 second)

-- fits a sequence to a given time signature
fit :: Rational -> Motif -> Motif
fit time motif =
  let missing = getMissing time motif
      missing1Short = getMissing time (init motif)
  in case signum missing of
    1         -> motif ++ [Rest (missing :: Dur)]
    (-1)      -> if signum missing1Short == 1 -- if its too short with one less note
                  then  init motif ++ [changeDur missing (last motif)]
                else fit time (init motif) -- remove last note..
    otherwise -> motif

changeDur :: Dur -> Primitive Pitch -> Primitive Pitch
changeDur diff (Rest dur) = Rest (dur + diff)
changeDur diff (Note dur p) = Note (dur + diff) p

data Order = RI | FA-- RISING, FALLING

reorder :: Order -> Motif -> Motif
reorder RI motif = replacePitches (map pitch $ absPSort motif) motif
reorder FA motif =
  replacePitches (map pitch $ Data.List.reverse $ absPSort motif) motif


absPSort motif = sort $ map absPitch $ getPitches motif

-- HELPER FUNCTIONS ------------------------------------------------------------

getMissing :: Rational -> Motif -> Rational
getMissing time motif =
  let motifDur = getTotalDur motif
      missing = time - motifDur
  in missing

cTrans :: AbsPitch -> Motif -> Motif
cTrans ap motif = map (cPrimTrans ap) motif

cPrimTrans :: AbsPitch -> Primitive Pitch -> Primitive Pitch
cPrimTrans ap (Note dur p) = Note dur (pitch $ absPitch p + ap)
cPrimTrans ap (Rest dur) = (Rest dur)

getTotalDur :: Motif -> Rational
getTotalDur motif = sum $ map getDur motif

-- primTrans: Transposition of a Primitive Pitch by a given amt of Scale Degrees
-- NB: fails if pitch is outside of absPitch (0,127) i.e MIDI scale (use Maybe)
primTrans :: Scale -> ScaleDeg -> Primitive Pitch -> Primitive Pitch
primTrans _ _ (Rest dur) = Rest dur
primTrans scale steps (Note dur p) =
  let transposedSD =  (toSD scale (absPitch p)) + steps
      transposedAP = toAP scale transposedSD
  in Note dur (pitch transposedAP)

-- invertSD: Diatonic inversion of sequence of Scale Degrees around first note
invertSD :: [ScaleDeg] -> [ScaleDeg]
invertSD motifSD =
  let pitchAxis = cycle $ [head motifSD]
      invInterSD = map (*(-1)) $ zipWith (-) motifSD pitchAxis
  in zipWith (+) pitchAxis invInterSD

changelastSD :: Scale -> ScaleDeg -> Motif -> Motif
changelastSD scale deg motif =
  let pitches = getPitches motif
      lastPitchSD = toSD scale $ absPitch $ last pitches
      nearestSD = (nearestMultiple 7 (lastPitchSD - deg)) + deg
      nearestP = pitch $ toAP scale nearestSD
  in replacePitches (init pitches ++ [nearestP]) motif

movelastSD :: Scale -> ScaleDeg -> Motif -> Motif
movelastSD scale deg motif =
  let pitches = getPitches motif
      lastPitchSD = toSD scale $ absPitch $ last pitches
      newLastPitchSD = lastPitchSD + deg
      newLastPitchP = pitch $ toAP scale newLastPitchSD
  in replacePitches (init pitches ++ [newLastPitchP]) motif

replaceDuration :: Dur -> Primitive Pitch -> Primitive Pitch
replaceDuration newDur (Rest dur) = Rest newDur
replaceDuration newDur (Note dur p) = Note newDur p

replacePitch :: Pitch -> Primitive Pitch -> Primitive Pitch
replacePitch _ (Rest dur) = Rest dur
replacePitch newP (Note dur p) = Note dur newP

getDur :: Primitive Pitch -> Dur
getDur (Note dur _) = dur
getDur (Rest dur) = dur

getPitch :: Primitive Pitch -> Maybe Pitch
getPitch (Note _ p) = Just p
getPitch (Rest _) = Nothing

getPitches :: Motif -> [Pitch]
getPitches motif =
  let pitches = map getPitch motif
  in catMaybes $ pitches

toSD scale = fromJust . Scale.toScaleDeg scale
toAP scale = fromJust . Scale.toAbsPitch scale

notRest (Rest _ ) = False
notRest (Note _ _) = True

nearestMultiple :: Int -> Int -> Int
nearestMultiple n number = round(fromIntegral number/fromIntegral n) * n

-- TESTING ---------------------------------------------------------------------
