import Euterpea
import System.Random


main :: IO ()
main = do
  play_piece
  --play_other_piece
  get_devices

play_piece =
    let
      m1 :: [AbsPitch]
      m1 = cycle [60, 60, 60, 64, 65, 65, 65, 65]
    in play_simple m1


play_other_piece =
  playDev 4 m
    where
      m1 :: [AbsPitch]
      m1 = [60, 56, 56, 56, 65, 65, 65, 65]
      m = line $ map (note hn) m1


play_simple bl =
  playDev 5 m
    where
      m = line $ map (note hn) [x + 3 | x <- bl]


get_devices = devices
