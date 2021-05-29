module Random
( getRandoms
, getRandomss
) where

import Control.Monad.State
import System.Random

--gets one random element from a list:
getRandom :: (RandomGen g) => [a] -> g -> (a, g)
getRandom list seed =
  let randomIdx = randomR (0,(length list) -1) seed
  in (list !! fst randomIdx, snd randomIdx)

-- gets n random elements from a list:
getRandoms :: Int -> [a] -> State StdGen [a]
getRandoms n = sequence . replicate n . state . getRandom

-- gets one random element from each list in a list of lists:
getRandomss ::  [[a]] -> State StdGen [a]
getRandomss =  sequence . map (state . getRandom)
