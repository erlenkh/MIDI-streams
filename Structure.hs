import Euterpea

{-
 the grouping structure of a piece, represented as a
 polymorphic tree, with Atoms (Notes or rests) as leaves.
 Each branch is labeled with an orientation (chord or line)
-}

--data Atom = Noto (Music Pitch, Volume) | Euterpea.Rest Dur

data Orientation = H | V deriving (Show) -- Horizontal | Vertical

data OrientedTree a = Val a | Group Orientation [OrientedTree a]
  deriving (Show)


-- figure out a way to deal with primitives..


-- 2 alternatives: create an Event-like variable
-- Use Euterpea..
