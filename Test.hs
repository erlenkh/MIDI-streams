-- an area to play around with different modules. testing etc.
import Scale
import Structure
import Euterpea

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
