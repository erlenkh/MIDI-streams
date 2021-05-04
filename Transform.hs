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
, Transform.replaceDurations
, Transform.replaceDuration
, Transform.movelastSD
, Transform.cTrans
, getDur
, reorder
, Order (..)
, fit
, getPitches
) where

import Euterpea
import Scale
import Data.List
import Data.Maybe

-- MOTIF TRANSFORMATION IN SCALE CONTEXT --------------------------------------

--TODO: Clean up, rename to comply with standards, and handle Maybe better...

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
      motifSD = map (toSD root mode) $ map (absPitch) motifP
      invMotifSD = invertSD motifSD
      invMotifP = map (pitch . toAP root mode) $ invMotifSD
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
primTrans :: Root -> Mode -> ScaleDeg -> Primitive Pitch -> Primitive Pitch
primTrans _ _ _ (Rest dur) = Rest dur
primTrans root mode steps (Note dur p) =
  let transposedSD =  (toSD root mode (absPitch p)) + steps
      transposedAP = toAP root mode transposedSD
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
      lastPitchSD = toSD root mode $ absPitch $ last pitches
      nearestSD = (nearestMultiple 7 (lastPitchSD - deg)) + deg
      nearestP = pitch $ toAP root mode nearestSD
  in replacePitches (init pitches ++ [nearestP]) motif

movelastSD :: Root -> Mode -> ScaleDeg -> Motif -> Motif
movelastSD root mode deg motif =
  let pitches = getPitches motif
      lastPitchSD = toSD root mode $ absPitch $ last pitches
      newLastPitchSD = lastPitchSD + deg
      newLastPitchP = pitch $ toAP root mode newLastPitchSD
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

toSD root mode = fromJust . Scale.toScaleDeg root mode
toAP root mode = fromJust . Scale.toAbsPitch root mode

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
