-- an area to play around with different modules. testing etc.
import Scale
import Structure
import Euterpea

exampleSong :: OrientedTree (Primitive Pitch)
exampleSong = Group H [
                Group V [
                  Val (Note qn (C,4)),
                  Val (Note qn (D,4)),
                  Val (Note qn (E,4))
                  ],
                Group V [
                  Val (Note qn (F,4)),
                  Val (Note qn (G,4)),
                  Val (Note qn (A,4))
                  ]
              ]
