module Scale
( getScaleNotes
, createFullMIDIScale
, scaleDeg2AbsPitch
, Root
) where

import Euterpea
import Data.List
import qualified Data.Map as Map

type Root = PitchClass

getScaleNotes :: Root -> Octave -> Mode -> [Pitch]
getScaleNotes root octave mode =
  let modeIntervals = getModeIntervals mode
      rootAbs = absPitch (root, octave)
      folding_func acc x = head acc + x : acc
      scaleAbsPitch = reverse $ foldl folding_func [rootAbs] modeIntervals
  in map pitch scaleAbsPitch

createFullMIDIScale :: Root -> Mode -> [AbsPitch]
createFullMIDIScale root mode =
  let octave_scale octave = init $ getScaleNotes root octave mode
      full_scale =  map absPitch $ concat $ map octave_scale [-1..9]
  in [x | x <- full_scale, x >= 0, x < 128] -- limit to MIDI range (0,127)

-- scale degrees are zero-indexed:
scaleDeg2AbsPitch ::  Root -> Mode -> Int -> AbsPitch
scaleDeg2AbsPitch  root mode  scaleDeg =
  let fullMIDIScale = createFullMIDIScale root mode
  in fullMIDIScale !! scaleDeg

getModeIntervals :: Mode -> [AbsPitch]
getModeIntervals mode =
  let majorIntervals = [2, 2, 1, 2, 2, 2, 1]
      -- the 7 Diatonic Modes: (Major == Ionian, Minor == Aeolian)
      modeOrder = [Major, Dorian, Phrygian, Lydian, Mixolydian, Minor, Locrian]
      modeIdxs = Map.fromList $ zip modeOrder [0..]
      modeIdx = modeIdxs Map.! mode
  in drop modeIdx majorIntervals ++ take modeIdx majorIntervals
