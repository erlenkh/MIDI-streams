import Example

import Euterpea
import System.Random

main = do
  gen <- newStdGen
  putStrLn $ show gen
  writeMidi (midiPath $ show gen) $ Example.genPiece gen
--  playDev 6 $ Example.genPiece gen
  -- |    ^ the midi channel you want to stream through

midiPath name =
  let folder = "/home/erlend/Documents/github/MIDI-streams/midi/"
  in  folder ++ name ++ ".mid"
