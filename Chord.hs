import Euterpea
import Scale


-- represent a triad as a root, third and a fifth

data Triad = Triad { root :: Pitch
                   , third :: Pitch
                   , fifth :: Pitch
                   } deriving (Show)
