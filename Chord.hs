import Euterpea
import Scale
import Data.List

-- a Chord consists of a root note and the pitches it is made of (incl. root)

data Chord = Chord { root :: PitchClass
                   , pitches :: [Pitch]
                   } deriving (Show)

getTriad :: Pitch -> Mode -> Chord
getTriad r@(rootPC, octave) mode =
  let scale = getScaleNotes rootPC mode octave
      third = scale !! 2
      fifth = scale !! 4
  in Chord {root = rootPC, pitches = [r,third, fifth]}

-- set inversion. works, but can be simplified
setInversion :: Int -> Chord -> Chord
setInversion 0 chord =
  let rootPitch@(r,rOct) = getRootPitch chord
      other = map absPitch $ filter (/= rootPitch) $ pitches chord
      (l,h) = splitBySize other (absPitch rootPitch)
      lAbove = map absPitch $ map (\(pc,o) -> (pc, rOct + 1)) $ map pitch l
      sortedOther = sort $ lAbove ++ h
  in chord {root = r, pitches = rootPitch : (map pitch sortedOther)}

setInversion x chord =
  let rootInversion = setInversion 0 chord
      (a,b) = splitAt x (pitches rootInversion)
      invertedPitches = b ++ (map (\(pc, o) -> (pc, o + 1)) a)
  in chord {root = root chord, pitches = invertedPitches}

getRootPitch :: Chord -> Pitch
getRootPitch chord =
  head $ filter (\(pc, oct) -> pc == root chord) $ pitches chord

splitBySize :: Ord a => [a] -> a -> ([a],[a])
splitBySize list x = (filter (< x) list , filter (> x) list)

-- TESTING ---------------------------------------------------------------------

cMajor = getTriad (C,4) Major

list = [1,2,3] :: [Int]

eMaj7 = Chord {root = E, pitches = [(Gs,4), (E,4), (B,4), (Ds,3)]}

-- simple test function, play chord
p ch =
  playDev 6 (chord $ map (\p -> Prim (p)) $ map (\p -> Note wn p) $ pitches ch)
