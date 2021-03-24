module Transform(
  Motif
, Transform.transpose
, Transform.reverse
, Transform.fullReverse
, Transform.invert
, Transform.replacePitch
, Transform.givePitches
, Transform.giveRhythm
, Transform.strongCadence
, Transform.weakCadence
) where

import Euterpea
import Scale
import Data.List

-- MOTIF TRANSFORMATION IN SCALE CONTEXT --------------------------------------

type Motif = [Primitive Pitch]

-- transpose: Transposition of Motif by a given amount of Scale Degrees
transpose :: Root -> Mode -> ScaleDeg -> Motif -> Motif
transpose root mode deg motif = map (primTrans root mode deg) motif

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
invert :: Root -> Mode -> Motif -> Motif
invert root mode motif =
  let motifP = getPitches motif -- removes rests
      motifSD = map (toScaleDeg root mode) $ map (absPitch) motifP
      invMotifSD = invertSD motifSD
      invMotifP = map (pitch . toAbsPitch root mode) $ invMotifSD
  in  replacePitches invMotifP motif  -- replace inv pitches in motif


-- gives pitches of a giver motif to a taker motif
givePitches :: Motif -> Motif -> Motif
givePitches giver taker = replacePitches (getPitches giver) taker

-- gives the rhythm (duration and rest info) of a giver motif to a taker motif
-- returns a new motif with the pitches of the taker and the rhythm of the giver
giveRhythm :: Motif -> Motif -> Motif
giveRhythm giver taker = replacePitches (getPitches taker) giver


strongCadence :: Root -> Mode -> Motif -> Motif
strongCadence root mode motif = changelastSD root mode 0 motif

weakCadence :: Root -> Mode -> Motif -> Motif
weakCadence root mode motif = changelastSD root mode 4 motif

-- replaces the durations
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

-- HELPER FUNCTIONS ------------------------------------------------------------

-- primTrans: Transposition of a Primitive Pitch by a given amt of Scale Degrees
-- NB: fails if pitch is outside of absPitch (0,127) i.e MIDI scale (use Maybe)
primTrans :: Root -> Mode -> ScaleDeg -> Primitive Pitch -> Primitive Pitch
primTrans _ _ _ (Rest dur) = Rest dur
primTrans root mode steps (Note dur p) =
  let transposedSD =  (toScaleDeg root mode (absPitch p)) + steps
      transposedAP = toAbsPitch root mode transposedSD
  in Note dur (pitch transposedAP)

-- invertSD: Diatonic inversion of sequence of Scale Degrees around first note
invertSD :: [ScaleDeg] -> [ScaleDeg]
invertSD motifSD =
  let pitchAxis = cycle $ [head motifSD]
      invInterSD = map (*(-1)) $ zipWith (-) motifSD pitchAxis
  in zipWith (+) pitchAxis invInterSD

changelastSD :: Root -> Mode -> ScaleDeg -> Motif -> Motif
changelastSD root mode deg motif =
  let pitches = getPitches motif
      lastPitchSD = toScaleDeg root mode $ absPitch $ last pitches
      nearestSD = (nearestMultiple 7 (lastPitchSD - deg)) + deg
      nearestP = pitch $ toAbsPitch root mode nearestSD
  in replacePitches (init pitches ++ [nearestP]) motif


replaceDuration :: Dur -> Primitive Pitch -> Primitive Pitch
replaceDuration newDur (Rest dur) = Rest newDur
replaceDuration newDur (Note dur p) = Note newDur p

replacePitch :: Pitch -> Primitive Pitch -> Primitive Pitch
replacePitch _ (Rest dur) = Rest dur
replacePitch newP (Note dur p) = Note dur newP

getMaybePitch :: Primitive Pitch -> Maybe Pitch
getMaybePitch (Note _ p) = Just p
getMaybePitch (Rest _) = Nothing

getPitches :: Motif -> [Pitch]
getPitches motif =
  let pitches = map getMaybePitch motif
  in map (\(Just x) -> x) $ filter (/= Nothing) $ pitches

toScaleDeg root mode = (\(Just x) -> x) . Scale.absPitch2ScaleDeg root mode
toAbsPitch root mode = (\(Just x) -> x) . Scale.scaleDeg2AbsPitch root mode

notRest (Rest _ ) = False
notRest (Note _ _) = True

nearestMultiple :: Int -> Int -> Int
nearestMultiple n number = round(fromIntegral number/fromIntegral n) * n

-- TESTING ---------------------------------------------------------------------

motif :: Motif
motif = [Note en (C,4), Rest sn, Note sn (C,4), Note en (E,4), Note en (B,4)]
durs = [qn, en, en, qn, qn]
motifP :: [Pitch]
motifP = [(G,4), (A,4), (G,4), (A,4)]

motif_inv = Transform.invert C Major motif
motif_trans2 = Transform.transpose C Major 2 motif
motif_rev = Transform.reverse motif

test_all = toM $ concat [motif, motif_inv, motif_trans2, motif_rev]
test_trans = toM $ concat $ map (\x -> Transform.transpose C Major x motif) [0..8]

toM motif = line $ map (\x -> Prim (x)) $ motif
