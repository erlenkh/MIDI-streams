import Example

import Euterpea
import System.Random

main = do
  gen <- newStdGen
  playDev 6 $ Example.piece gen
  -- |    ^ the midi channel you want to stream through
