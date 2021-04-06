module Scale
( getScaleNotes
, createFullMIDIScale
, scaleDeg2AbsPitch
, absPitch2ScaleDeg
, Root
, ScaleDeg
) where

import Euterpea
import Data.List
import qualified Data.Map as Map

type Root = PitchClass
type ScaleDeg = Int

getScaleNotes :: Root -> Octave -> Mode -> [Pitch]
getScaleNotes root octave mode =
  let modeIntervals = getModeIntervals mode
      rootAbs = absPitch (root, octave)
      folding_func acc x = head acc + x : acc
      scaleAbsPitch = reverse $ foldl folding_func [rootAbs] modeIntervals
  in map pitch scaleAbsPitch

createFullScale :: Root -> Mode -> [AbsPitch]
createFullScale root mode =
  let octave_scale octave = init $ getScaleNotes root octave mode
  in map absPitch $ concat $ map octave_scale [-1..9]

createFullMIDIScale :: Root -> Mode -> [AbsPitch]
createFullMIDIScale root mode =
  [x | x <- createFullScale root mode, x >= 0, x < 128]

-- scale degrees are zero-indexed:
scaleDeg2AbsPitch ::  Root -> Mode -> ScaleDeg -> Maybe AbsPitch
scaleDeg2AbsPitch  root mode  scaleDeg =
  if (scaleDeg >= 0 && scaleDeg < 128) then
   Just $ (createFullMIDIScale root mode) !! scaleDeg
  else Nothing

  -- scale degrees are zero-indexed:
absPitch2ScaleDeg :: Root -> Mode -> AbsPitch -> Maybe ScaleDeg
absPitch2ScaleDeg root mode absP =
  elemIndex absP $ createFullMIDIScale root mode

getModeIntervals :: Mode -> [AbsPitch]
getModeIntervals mode =
  let majorIntervals = [2, 2, 1, 2, 2, 2, 1]
      -- the 7 Diatonic Modes: (Major == Ionian, Minor == Aeolian)
      modeOrder = [Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian]
      modeIdxs = Map.fromList $ zip modeOrder [0..]
      modeIdx = modeIdxs Map.! mode
  in drop modeIdx majorIntervals ++ take modeIdx majorIntervals

  --TODO rethink scale representation. Maybe Scale root intervals
