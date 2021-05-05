module Scale
( scalePitches
, fullMIDIScale
, toAbsPitch
, toScaleDeg
, Root
, ScaleDeg
) where

  -- TODO: Make scale datatype...

import Euterpea
import Data.List
import qualified Data.Map as Map

type Root = PitchClass
type ScaleDeg = Int

data Scale = Scale Root [Int] -- scale?

scalePitches :: Root -> Mode -> Octave -> [Pitch]
scalePitches root mode octave =
  let modeIntervals = intervals mode
      rootAbs = absPitch (root, octave) 
      ff acc x = head acc + x : acc
      scaleAbsPitch = reverse $ foldl ff [rootAbs] modeIntervals
  in map pitch scaleAbsPitch

fullScale :: Root -> Mode -> [AbsPitch]
fullScale root mode =
  let octave_scale octave = init $ scalePitches root mode octave
  in map absPitch $ concat $ map octave_scale [-1..9]

fullMIDIScale :: Root -> Mode -> [AbsPitch]
fullMIDIScale root mode =
  [x | x <- fullScale root mode, x >= 0, x < 128]

-- scale degrees are zero-indexed:
toAbsPitch ::  Root -> Mode -> ScaleDeg -> Maybe AbsPitch
toAbsPitch  root mode  scaleDeg =
  if (scaleDeg >= 0 && scaleDeg < 128) then
   Just $ (fullMIDIScale root mode) !! scaleDeg
  else Nothing

  -- scale degrees are zero-indexed:
toScaleDeg :: Root -> Mode -> AbsPitch -> Maybe ScaleDeg
toScaleDeg root mode absP =
  elemIndex absP $ fullMIDIScale root mode

intervals :: Mode -> [AbsPitch]
intervals mode =
  let majorIntervals = [2, 2, 1, 2, 2, 2, 1]
      -- the 7 Diatonic Modes: (Major == Ionian, Minor == Aeolian)
      modeOrder = [Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian]
      modeIdxs = Map.fromList $ zip modeOrder [0..]
      modeIdx = modeIdxs Map.! mode
  in drop modeIdx majorIntervals ++ take modeIdx majorIntervals

  --TODO rethink scale representation. Maybe Scale root intervals
