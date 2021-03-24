import Scale
import Structure
import Euterpea
import qualified Transform as T

-- MUSIC TREE ------------------------------------------------------------------

type MusicTree = OrientedTree (Primitive Pitch)

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions
treeToMusic :: MusicTree -> Music Pitch
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H trees) = line (map treeToMusic trees)
treeToMusic (Group V trees) = chord (map treeToMusic trees)

valToMusic :: MusicTree -> Music Pitch
valToMusic (Val x) = Prim (x)

-- MUSIC PT --------------------------------------------------------------------
type MusicPT =
   PrefixTree (Slice -> Slice) (MusicTree -> MusicTree)

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

gt :: (T.Motif -> T.Motif) -> (MusicTree -> MusicTree)
gt f group@(Group o _) = toGroup o $ f $ fromGroup group

inv = gt $ T.invert C Major
rev  = gt $ T.reverse
transp x = gt $ T.transpose C Major x
givePs group = gt $ T.givePitches (fromGroup group)
giveR group = gt $ T.giveRhythm (fromGroup group)

-- TESTING MATERIAL: -----------------------------------------------------------
motif :: T.Motif
motif = [Note qn (C,4), Note qn (D,4), Note qn (E,4), Note qn (B,4)]
motif_inv = T.invert C Major motif
motif_trans2 = T.transpose C Major 2 motif
motif_rev = T.reverse motif

mel :: MusicTree
mel = Group H $ map (toGroup H) [motif, motif_inv, motif, motif_rev]

testPtree :: MusicPT
testPtree =  Leaf (atMotifs [0,1]) (replaceVal testOTree)

testOTree :: OrientedTree (Primitive Pitch)
testOTree =
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
