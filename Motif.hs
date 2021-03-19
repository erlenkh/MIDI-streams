module Motif
( Motif
, Motif.transpose
, Motif.reverse
, Motif.fullReverse
, Motif.invert
) where

import Euterpea
import Scale
import Data.List

type Motif = [Primitive Pitch]

-- TRANSFORMATION IN SCALE CONTEXT ---------------------------------------------

-- transpose: Transposition of Motif by a given amount of Scale Degrees
transpose :: Root -> Mode -> Motif -> ScaleDeg -> Motif
transpose root mode motif deg = map (primTrans root mode deg) motif

-- reverse: Reversion of pitch sequence but not durations or rests
reverse :: Motif -> Motif
reverse motif =
  let motifP = getPitches motif -- removes rests
      revMotifP = Data.List.reverse motifP
  in replacePitches motif revMotifP

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
      invMotif = replacePitches motif invMotifP -- replace inv pitches in motif
  in invMotif

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

-- replacePitches: takes a sequence of Pitches and a Motif and changes the
-- Pitches while preserving the Rests and durations.

replacePitches :: Motif -> [Pitch] -> Motif
replacePitches [] _ = []
replacePitches motif pitches  =
  let first = takeWhile (notRest) motif
      second = dropWhile (notRest) motif
      splitPs = splitAt (length first) pitches
      newPrimPs = zipWith replacePitch first (fst splitPs)
      rest = take 1 second
  in newPrimPs ++ rest ++ replacePitches (drop 1 second) (snd splitPs)

replacePitch :: Primitive Pitch -> Pitch -> Primitive Pitch
replacePitch (Rest dur) _ = Rest dur
replacePitch (Note dur p) newP = (Note dur newP)

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

-- TESTING ---------------------------------------------------------------------

motif :: Motif
motif = [Note en (C,4), Rest sn, Note sn (C,4), Note en (E,4), Note en (B,4)]
motifP :: [Pitch]
motifP = [(G,4), (A,4), (G,4), (A,4)]

motif_inv = Motif.invert C Major motif
motif_trans2 = Motif.transpose C Major motif 2
motif_rev = Motif.reverse motif

test_all = toM $ concat [motif, motif_inv, motif_trans2, motif_rev]
test_trans = toM $ concat $ map (Motif.transpose C Major motif) [0..8]

toM motif = line $ map (\x -> Prim (x)) $ motif
