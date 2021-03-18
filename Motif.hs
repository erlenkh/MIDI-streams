module Motif
( Motif.transpose
, Motif.reverse
) where

import Euterpea
import Scale
import Data.List

type Motif = [Primitive Pitch]
motif :: Motif

motif = [Note en (C,4), Note en (C,4), Note sn (E,4), Note en (B,4), Rest sn]

type ScaleDeg = Int

{-
--inverts around first note
invert :: Root -> Mode -> Motif -> Motif
invert root mode motif =
  let motifAbs = map primAbs motif
-}
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

getAbsIntervals :: Motif -> [AbsPitch]
getIntervals motif =
  let primAbsMotif = map (primAbs2Abs . primAbs) motif
      absMotif = map (\(Just x) -> x) $ filter (/= Nothing) $ primAbsMotif
  in getdif absMotif

getdif :: [Int] -> [Int]
getdif x = zipWith (-) (tail x) x
