import Euterpea
import System.Random


main :: IO ()
main = do
  play_simple
  get_devices

play_simple =
  playDev 3 m
    where
      m1 :: [AbsPitch]
      m1 = cycle [60, 60, 60, 64, 65, 65, 65, 65]
      m = line $ map (note hn) m1

get_devices = devices
