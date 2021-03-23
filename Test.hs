-- an area to play around with different modules. testing etc.
import Scale
import Structure
import Euterpea
import qualified Transform as T

chordPart :: OrientedTree (Primitive Pitch)
chordPart =
              Group H [
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Val (Note qn (B,4)),
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ],
                Val (Rest hn)
                ]

song :: OrientedTree (Primitive Pitch)
song = Group H [chordPart, chordPart]

newSong = replaceElement song [0,1] $ Val (Note qn (E,4))

motif :: T.Motif
motif = [Note qn (C,4), Note qn (D,4), Note qn (E,4), Note qn (B,4)]

motif_inv = T.invert C Major motif
motif_trans2 = T.transpose C Major motif 2
motif_rev = T.reverse motif

mel :: MusicTree
mel = Group H $ map (toGroup H) [motif, motif_inv, motif, motif_rev]

mel2 = applyFunction (T.replacePitch ((G,3) :: Pitch)) [Some [1], Some [3]] mel
mel3 = applyFunction (T.replacePitch ((C,4) :: Pitch)) [Some [3], Some [3]] mel2
