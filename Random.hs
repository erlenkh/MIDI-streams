module Random (getRandoms, randomSt) where

import Control.Monad.State
import System.Random

-- RANDOM: ---------------------------------------------------------------------

getRandoms :: Int -> [a] -> State StdGen [a]
getRandoms x list = sequence $ replicate x (randomSt list)

randomSt :: (RandomGen g) => [a] -> State g a
randomSt list = state $ getRandom list

getRandom :: (RandomGen g) => [a] -> g -> (a, g)
getRandom list seed =
  let randomIdx = randomR (0,(length list) -1) seed
  in (list !! fst randomIdx, snd randomIdx)
