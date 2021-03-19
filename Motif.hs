module Motif
( Motif
, Motif.transpose
, Motif.reverse
, Motif.invert
) where

import Euterpea
import Scale
import Data.List


type Motif = [Primitive Pitch]

motif :: Motif
motif = [Rest sn, Note sn (C,4), Rest sn, Rest sn, Note en (D,4), Rest sn]

type ScaleDeg = Int

-- invert: Diatonic inversion of Motif around first note, preserving Rests
invert :: Root -> Mode -> Motif -> Motif
invert root mode motif =
  let motifSD = map (toScaleDeg root mode) $ getAbsMotif motif -- to scale deg
      invMotifSD = invertSD motifSD -- invert scale deg
      invMotifP = map (pitch . toAbsPitch root mode) $ invMotifSD -- to pitches
      invMotif = replacePitches motif invMotifP -- replace inv pitches in motif
  in invMotif

-- invertSD: Diatonic inversion of sequence of Scale Degrees around first note
invertSD :: [ScaleDeg] -> [ScaleDeg]
invertSD motifSD =
  let pitchAxis = cycle $ [head motifSD]
      invInterSD = map (*(-1)) $ zipWith (-) motifSD pitchAxis
  in zipWith (+) pitchAxis invInterSD

-- reverse: Reversion of Motif
reverse :: Motif -> Motif
reverse motif = Data.List.reverse motif

-- reverse: Transposition of Motif by a given amount of Scale Degrees
transpose :: Root -> Mode -> Motif -> ScaleDeg -> Motif
transpose root mode motif deg = map (primTrans root mode deg) motif

-- primTrans: Transposition of a Primitive Pitch by a given amt of Scale Degrees
-- NB: fails if pitch is outside of absPitch (0,127) i.e MIDI scale (use Maybe)
primTrans :: Root -> Mode -> ScaleDeg -> Primitive Pitch -> Primitive Pitch
primTrans root mode deg (Rest dur) = Rest dur
primTrans root mode deg (Note dur p) =
  let fullMIDIScale = createFullMIDIScale root mode
      Just aPIdx = elemIndex (absPitch p) fullMIDIScale
      transAPIdx = aPIdx + deg
      transAP = fullMIDIScale !! transAPIdx
  in Note dur (pitch transAP)


-- HELPER FUNCTIONS ------------------------------------------------------------

-- replacePitches: takes a sequence of Pitches and a Motif and changes the
-- Pitches while preserving the Rests as they were.
replacePitches :: Motif -> [Pitch] -> Motif
replacePitches motifM motifP = concat $ splitAndReplace motifM motifP

splitAndReplace :: Motif -> [Pitch] -> [Motif]
splitAndReplace [] _ = []
splitAndReplace motif pitches  = filter (/= []) $
  let primPs = takeWhile (notRest) motif
      splitPs = splitAt (length primPs) pitches
      newPrimPs = zipWith replacePitch primPs (fst splitPs)
      second = dropWhile (notRest) motif
      rest = head second
  in case length second of
        1 -> newPrimPs : [[rest]]
        0 -> newPrimPs : []
        otherwise ->
          newPrimPs : [rest] : splitAndReplace (tail second) (snd splitPs)

replacePitch :: Primitive Pitch -> Pitch -> Primitive Pitch
replacePitch (Rest dur) _ = Rest dur
replacePitch (Note dur p) newP = (Note dur newP)

primAbs :: Primitive Pitch -> Primitive AbsPitch
primAbs (Note dur p) = (Note dur (absPitch p))
primAbs (Rest dur) = Rest dur

primAbs2Abs :: Primitive AbsPitch -> Maybe AbsPitch
primAbs2Abs (Note dur ap) = Just ap
primAbs2Abs (Rest dur) = Nothing

getAbsMotif :: Motif -> [AbsPitch]
getAbsMotif motif =
  let primAbsMotif = map (primAbs2Abs . primAbs) motif
  in map (\(Just x) -> x) $ filter (/= Nothing) $ primAbsMotif

getDiffs :: [Int] -> [Int]
getDiffs x = zipWith (-) (tail x) x

toScaleDeg root mode = (\(Just x) -> x) . absPitch2ScaleDeg root mode
toAbsPitch root mode = (\(Just x) -> x) . scaleDeg2AbsPitch root mode

notRest (Rest _ ) = False
notRest (Note _ _) = True
