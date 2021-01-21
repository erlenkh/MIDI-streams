import Euterpea
import System.Random


main :: IO ()
main = do
  get_devices
  play_piece

play_piece =
    let
      m1 :: [AbsPitch]
      m1 = [69, 65, 69, 65, 69, 65, 69]
      m2 :: [AbsPitch]
      m2 = [60, 60, 60, 60, 60, 65, 65]
    in play_simple m1 m2

play_simple m1 m2 =
  playDev 4 piece
    where
      m = line $ map (note qn) m1
      n = line $ map (note qn) m2
      first = forever (m :+: rest qn :+: n :+: rest qn)
      second = forever ( n :+: rest qn)
      third = offset tn first
      piece = first :=: second :=: third
get_devices = devices
