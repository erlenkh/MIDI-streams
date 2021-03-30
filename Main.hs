import System.Random
import Composer
import Structure
import Transform
import Euterpea
main = do
  gen <- newStdGen
  let fp = Main.period gen motif4
  let newgen = snd $ m gen
  let tranz = applyGT [Some[1,3]] (ro FA . giveR m2) . applyGT [Some[0,2]] (ro RI) . applyGT [All] (giveR m3 . toGroup H . fromGroup)
  print (tranz chords)
  playDev 2 $ forever $ (treeToMusic $ (tranz) chords)  :=: (treeToMusic $ applyGT [All] (giveR m4) chords) -- :=: (treeToMusic $ fp)

t = getRandom transformations
m = getRandom motifs

motif4 = Transform.transpose C Major (- 7) $ extend 1 motif
motif = [Note qn (C,4), Rest en, Note en (C,4), Note qn (A,3)]
motif2 = [Note qn (C,4), Note qn (C,4), Note qn (B,4), Note qn (E,4)]
motif3 = [Note sn (A,4), Note sn (A,4), Note qn (B,4), Note qn (C,4)]

motifs = [motif, motif2, motif3]

m1, m2, m3, m4:: MusicTree
m1 = toGroup H [Note hn (C,3), Note qn (E,3), Note qn (F,3)]
m2 = toGroup H [Note qn (C,2), Note hn (D,2), Note qn (E,2)]
m3 = toGroup H [Note hn (C,4), Note qn (D,4), Note qn (B,4)]
m4 = toGroup H [Note wn (C,4), Note wn (D,4), Note wn (B,4)]
base m = (Group H $ map (toGroup H) $  replicate 4 m) :: MusicTree

period :: StdGen -> [Primitive Pitch] -> MusicTree
period gen motif = applyGT [Some[1]] (weak . fst (t gen)) $
  applyGT [Some[3]] (strong . fst (t gen) ) $
  applyGT [Some[1,3]] (giveR m1) $
   base motif

section = (toGroup H motif)

per gen =  applyGT [Some[1]] (weak . fst (t gen)) .
  applyGT [Some[3]] (strong . fst (t gen) ) .
  applyGT [Some[1,3]] (giveR m1)

transformations = [inv, rev, transp (-1), transp 1, transp 3, transp(-2)]

getRandom list seed =
  let randomIdx = randomR (0,(length list) -1) seed
  in (list !! fst randomIdx, snd randomIdx)

chords :: OrientedTree (Primitive Pitch)
chords =
              Group H [
                Group V [
                  Val (Note hn (E,5)),
                  Val (Note hn (A,4)),
                  Val (Note hn (C,5))
                  ],
                Group V [
                  Val (Note hn (C,5)),
                  Val (Note hn (F,4)),
                  Val (Note hn (A,4))
                  ],
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (E,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ]
                ]
