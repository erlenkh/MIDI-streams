module Motif
( Motif.transpose
, Motif.reverse
) where

import Euterpea
import Scale
import Data.List



type Motif = [Primitive Pitch]
motif :: Motif

motif = [Note en (E,4), Note en (C,4), Note sn (E,4), Note en (B,4), Rest sn]

type ScaleDeg = Int


-- Diatonic inversion around first note
invert :: Root -> Mode -> Motif -> [ScaleDeg]
invert root mode motif =
  let motifSD = map (toScaleDeg root mode) $ getAbsMotif motif
      invMotifSD = invertSD motifSD
      invMotifAbs = map (toAbsPitch root mode) $ invMotifSD
  in invMotifAbs

toScaleDeg root mode = (\(Just x) -> x) . absPitch2ScaleDeg root mode
toAbsPitch root mode = (\(Just x) -> x) . scaleDeg2AbsPitch root mode



splitOnRest :: [Primitive Pitch] -> [Primitive Pitch]
splitOnRest [] = []
splitOnRest motif  =
  let notRest (Rest _ ) = False
      notRest (Note _ _) = True
  in (takeWhile (notRest) motif)

{-
replacePitches :: Motif -> [Pitch] -> Motif
replacePitches motif motifP =
-}

replacePitch :: Primitive Pitch -> Pitch -> Primitive Pitch
replacePitch (Rest dur) _ = Rest dur
replacePitch (Note dur p) newP = (Note dur newP)

invertSD :: [ScaleDeg] -> [ScaleDeg]
invertSD motifSD =
  let pitchAxis = cycle $ [head motifSD]
      invInterSD = map (*(-1)) $ zipWith (-) motifSD pitchAxis
  in zipWith (+) pitchAxis invInterSD



reverse :: Motif -> Motif
reverse motif = Data.List.reverse motif

transpose :: Root -> Mode -> Motif -> ScaleDeg -> Motif
transpose root mode motif deg = map (primTrans root mode deg) motif

-- NB: primTrans fails if pitch is outside of absPitch (0,127) i.e MIDI scale
-- in future: implement with Maybe
primTrans :: Root -> Mode -> ScaleDeg -> Primitive Pitch -> Primitive Pitch
primTrans root mode deg (Rest dur) = Rest dur
primTrans root mode deg (Note dur p) =
  let fullMIDIScale = createFullMIDIScale root mode
      Just aPIdx = elemIndex (absPitch p) fullMIDIScale
      transAPIdx = aPIdx + deg
      transAP = fullMIDIScale !! transAPIdx
  in Note dur (pitch transAP)

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
