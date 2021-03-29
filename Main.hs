import System.Random
import Composer
import Structure
import Transform
import Euterpea
main = do
  gen <- newStdGen
  let fp = Main.period gen motif4
  let newgen = snd $ m gen
  let sp = Main.period newgen motif4
  playDev 2 $ treeToMusic (Group H [fp, sp])

t = getRandom transformations
m = getRandom motifs

motif4 = Transform.transpose C Major (- 7) $ extend 1 motif
motif = [Note qn (C,4), Rest en, Note en (C,4), Note qn (A,3)]
motif2 = [Note qn (C,4), Note qn (C,4), Note qn (B,4), Note qn (E,4)]
motif3 = [Note sn (A,4), Note sn (A,4), Note qn (B,4), Note qn (C,4)]

motifs = [motif, motif2, motif3]

m1, m2, m3 :: MusicTree
m1 = toGroup H [Note hn (C,3), Note qn (E,3), Note qn (F,3)]
m2 = toGroup H [Note qn (C,2), Note hn (D,2), Note qn (E,2)]
m3 = toGroup H [Note qn (C,4), Note qn (D,4), Note hn (B,4)]

base m = (Group H $ map (toGroup H) $  replicate 4 m) :: MusicTree

period :: StdGen -> [Primitive Pitch] -> MusicTree
period gen motif = applyGT [Some[1]] (weak . fst (t gen)) $
  applyGT [Some[3]] (strong . fst (t gen) ) $
  applyGT [Some[1,3]] (giveR m1) $
   base motif

section = (toGroup H motif)

transformations = [inv, rev, transp (-1), transp 1, transp 3, transp(-2)]

getRandom list seed =
  let randomIdx = randomR (0,(length list) -1) seed
  in (list !! fst randomIdx, snd randomIdx)
