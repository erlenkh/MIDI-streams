import Example

import Euterpea
import System.Random

main = do
  gen <- newStdGen
  putStrLn $ show gen 
  playDev 6 $ Example.genPiece gen
  -- |    ^ the midi channel you want to stream through
