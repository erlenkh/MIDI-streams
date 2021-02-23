-- an area to play around with different modules. testing etc.
import Scale
import Structure
import Euterpea

exampleSong2 :: OrientedTree (Primitive Pitch)
exampleSong2 = Group H [
                Val (Note en (C,4)),
                Val (Rest en),
                Group V [
                  Val (Note qn (C,4)),
                  Val (Note qn (E,4)),
                  Group H [
                    Val (Note en (G,4)),
                    Val (Rest en)
                  ]
                ]
              ]
