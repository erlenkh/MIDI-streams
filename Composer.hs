import Scale
import Structure
import Euterpea
import qualified Transform as T

-- MUSIC TREES ------------------------------------------------------------------

type MusicTree = OrientedTree (Primitive Pitch)

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions
treeToMusic :: MusicTree -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H trees) = line (map treeToMusic trees)
treeToMusic (Group V trees) = chord (map treeToMusic trees)

valToMusic :: MusicTree -> Music Pitch
valToMusic (Val x) = Prim (x)

type MusicPT = PrefixTree (Slice -> Slice) (MusicTree -> MusicTree)

-- SLICE CONSTRUCTION ----------------------------------------------------------

-- slice construction: allows the composition of (Slice -> Slice)
-- examples that apply to "testTree": (need to be generalized)
-- should they add? i.e. atVoices[0,1] . atVoices[2] = atVoices [0,1,2]?
-- right now atVoices[0,1] . atVoices[2] = atVoices [0,1]
atMotifs, atChords, atVoices :: [Int] -> Slice -> Slice
atMotifs selection [_, chords, voices] = [Some selection, chords, voices]
atChords selection [motifs, _ , voices] = [motifs, Some selection, voices]
atVoices selection [motifs, chords, _] = [motifs, chords, Some selection]

-- GROUP TRANSFORMATIONS: ------------------------------------------------------

toGT :: (T.Motif -> T.Motif) -> (MusicTree -> MusicTree)
toGT f group@(Group o _) = toGroup o $ f $ fromGroup group

inv = toGT $ T.invert C Major
rev  = toGT $ T.reverse
transp x = toGT $ T.transpose C Major x
givePs group = toGT $ T.givePitches (fromGroup group)
giveR group = toGT $ T.giveRhythm (fromGroup group)
strong = toGT $ T.strongCadence C Major
weak = toGT $ T.weakCadence C Major


-- TESTING ZONE: ---------------------------------------------------------------

p tree = playDev 6 (treeToMusic tree) --quick play

motif, motif2 :: T.Motif
motif = [Note qn (C,4), Note qn (D,4), Note qn (E,4), Note qn (B,4)]
motif2 = [Note qn (C,4), Note qn (C,4), Note qn (B,4), Note qn (E,4)]

base m = (Group H $ map (toGroup H) $  replicate 4 m) :: MusicTree
period = applyGT [Some[1]] (weak. transp (-2)) .
  applyGT [Some[3]] (strong . inv) . applyGT [Some[1,3]] (giveR m3) . base

m1, m2, m3 :: MusicTree
m1 = toGroup H [Note hn (C,3), Note qn (E,3), Note qn (F,3)]
m2 = toGroup H [Note qn (C,2), Note hn (D,2), Note qn (E,2)]
m3 = toGroup H [Note qn (C,4), Note qn (D,4), Note hn (B,4)]

testPT :: MusicPT
testPT =  Leaf (atMotifs [0,1]) inv

chords :: OrientedTree (Primitive Pitch)
chords =
              Group H [
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ]
                ]
