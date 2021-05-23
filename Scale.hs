module Scale
( scaleAbsPitches
, fullMIDIScale
, toAbsPitch
, toScaleDeg
, Root
, ScaleDeg
, Scale (..)
) where

import Euterpea
import Data.List
import qualified Data.Map as Map
import Data.Maybe

type Root = PitchClass
data Scale = Scale Root Mode

scaleAbsPitches :: Scale -> Octave -> [AbsPitch]
scaleAbsPitches (Scale root mode) octave =
  let rootAbs = absPitch (root, octave)
      ff acc x = acc ++ [last acc + x]
  in  foldl ff [rootAbs] (intervals mode)

fullScale :: Scale -> [AbsPitch]
fullScale scale =
  let octave_scale octave = init $ scaleAbsPitches scale octave
  in concat $ map octave_scale [-1..]

fullMIDIScale :: Scale -> [AbsPitch]
fullMIDIScale scale = [x | x <- fullScale scale, x >= 0, x < 128]

intervals :: Mode -> [Int]
intervals mode =
  let majorIntervals = [2, 2, 1, 2, 2, 2, 1]
      mIdx = modeIdx mode
  in drop mIdx majorIntervals ++ take mIdx majorIntervals

modeIdx :: Mode -> Int
modeIdx mode =
  let modes = [Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian]
  -- ^ the 7 Diatonic Modes (Major == Ionian, Minor == Aeolian)
  in fromJust $ elemIndex mode modes

type ScaleDeg = Int

-- scale degrees are zero-indexed:
toAbsPitch ::  Scale -> ScaleDeg -> Maybe AbsPitch
toAbsPitch  scale scaleDeg =
  if (scaleDeg >= 0 && scaleDeg < 128) then
   Just $ (fullMIDIScale scale) !! scaleDeg
  else Nothing

  -- scale degrees are zero-indexed:
toScaleDeg :: Scale -> AbsPitch -> Maybe ScaleDeg
toScaleDeg scale absP = elemIndex absP $ fullMIDIScale scale
