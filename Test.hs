import Control.Monad.State

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)
push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  return [a1, a2, a3]

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
  in ([a1, a2, a3], s3)

mondayS' =
  coinS >>= (\ a1 -> pushS >>= (\ a2 -> pushS >>= (\ a3 -> return [a1, a2, a3])))
