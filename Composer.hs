module Composer
( MusicTree (..)
, treeToMusic
, inv
, rev
, transp
, giveR
, strong
, weak
, ro
)
where

import Scale
import Structure
import Euterpea
import qualified Transform as T
import Chord
-- MUSIC TREES ------------------------------------------------------------------

type MusicTree = OrientedTree (Primitive Pitch)

-- converts from a piece of music from orientedTree to Euterpeas 'Music Pitch'
-- enables us to play the piece as MIDI with built-in Euterpea functions
treeToMusic :: MusicTree -> Music (Pitch, Volume)
treeToMusic (Val x) = valToMusic (Val x)
treeToMusic (Group H trees) = line (map treeToMusic trees)
treeToMusic (Group V trees) = chord (map treeToMusic trees)

valToMusic :: MusicTree -> Music (Pitch, Volume)
valToMusic (Val (Note dur p)) = Prim ((Note dur (p, 75)))
valToMusic (Val (Rest dur)) = Prim (Rest dur)

type MusicPT = PrefixTree GT (Slice -> Slice)

-- SLICE CONSTRUCTION ----------------------------------------------------------

-- slice construction: allows the composition of (Slice -> Slice)
-- examples that apply to "testTree": (need to be generalized)
-- should they add? i.e. atVoices[0,1] . atVoices[2] = atVoices [0,1,2]?
-- right now atVoices[0,1] . atVoices[2] = atVoices [0,1]


-- GROUP TRANSFORMATIONS: ------------------------------------------------------
type GT = MusicTree -> MusicTree

toGT :: (T.Motif -> T.Motif) -> GT
toGT f = applySF f

inv = toGT $ T.invert C Major
rev  = toGT $ T.reverse
transp x = toGT $ T.transpose C Major x
givePs group = toGT $ T.givePitches (fromGroup group)
giveR group = toGT $ T.giveRhythm (fromGroup group)
strong = toGT $ T.strongCadence C Major
weak = toGT $ T.weakCadence C Major
ro = toGT . T.reorder
insert new old = new
mlSD x = toGT $ T.movelastSD C Major x
ct = toGT . T.cTrans

invGT :: MusicTree -> MusicTree
invGT = applySF $ T.invert C Major

-- TRANSFORMATIVE INSTRUCTIONS -------------------------------------------------

data TI = TI { slc :: Slice, gt :: GT} -- Transformative Instruction

applyTIs :: [TI] -> MusicTree -> MusicTree
applyTIs instructions startingTree =
  foldl (flip applyTI) startingTree instructions

applyTI :: TI -> MusicTree -> MusicTree
applyTI (TI slice gt) tree = applyGT slice gt tree

tis2Tree :: [TI] -> MusicTree
tis2Tree instructions = applyTIs instructions (makeStartingTree instructions)

toTI :: Slice -> ([Slice -> Slice], GT) -> TI
toTI levels (strans, gtrans) =
   TI {slc = foldl(\acc f -> f acc) levels strans, gt = gtrans }

pt2Tree :: MusicPT -> MusicTree
pt2Tree pt =
  let levels = [All, All, All]
      values = getAllValues $ fmap ($ levels) pt
      paths = getAllPaths pt
  in tis2Tree $ map (toTI levels) $ zip paths values


-- TESTING ZONE: ---------------------------------------------------------------

atPeriods selection [a,b,c] = [Some selection, b, c]
atPhrases selection [a,b,c] = [a, Some selection,c]
atMeasures selection [a,b,c] = [a,b, Some selection]

p tree = playDevS 6 $ tempo 0.80 $ (treeToMusic tree) --quick play

mkChord pitch mode dur = map (\p -> Note dur p) $ pitches $ getTriad pitch mode

mc p o m = insert $ toGroup V $ mkChord (p,o) m hn


cv_chords = Node (atPeriods [0]) [
              Node (atMeasures[0,1]) [
                  Leaf (atPhrases [0]) (mc C 3 Major)
              ,   Leaf (atPhrases [1]) (mc A 2 Minor)
              ,   Leaf (atPhrases [2]) (mc F 2 Major)
              ,   Node (atPhrases [3]) [
                      Leaf (atMeasures[0]) (mc D 3 Minor)
                  ,   Leaf (atMeasures[1]) (mc G 2 Major)
                  ]
              ]
            , Leaf (atPhrases [0,1,2,3]) (toCV)
            , Node (atMeasures[1]) [
                Leaf (atPhrases [0,1] . atMeasures[1]) (mlSD (-1))
            ]
            ]

toCV :: MusicTree -> MusicTree
toCV (Group V notes) =
  let Val (Note dur root) = head notes
      voice1 = [Note en root] :: T.Motif
      voice2 = [Note en (C,4)] :: T.Motif
      f = concat . replicate 4
  in Group V [  Group H (map toVal $ f voice1)
             ,  Group H ( map toVal $ T.fit 0.5 $ (Rest sn :: Primitive Pitch) : f voice2)
             ]

toVal = (\x -> Val x)

{-
toCV :: MusicTree -> MusicTree
toCV (Group V notes) =
  let Val (Note dur root) = head notes
      rhytmicroot = [Note sn root, Note sn (C,4)] :: T.Motif
  in Group H (map (\x -> Val x) $ concat $ replicate 4 rhytmicroot)
-}

-- simplified version of "cellevevet"

{-
atVoices, atPeriods, atMotifs :: [Int] -> Slice -> Slice
atVoices selection [a, b, c] = [Some selection, b, c]
atPeriods selection [a, b, c] = [a, Some selection, c]
atMotifs selection [a, b, c] = [a, b, Some selection ]

cvMotif :: MusicTree
cvMotif = toGroup H [Note en (C,4), Note en (C,4), Note en (C,4), Note en (C,4)]
cvMotif2 = toGroup H [Rest sn, Note en (C,4), Note en (C,4), Note en (C,4), Note sn (C,4)]

cv = Node (atMotifs [0,1,2,3,4,5,6,7]) [
        Node (atVoices [0]) [
           Leaf (atPeriods [0,1,2,3,4,5,6,7,8]) (insert $ transp (-7) cvMotif)
        ,  Node (atPeriods [0,1,2,3,7,8]) [
              Leaf (atMotifs [2,3]) (transp (-2))
           ,  Leaf (atMotifs [4,5]) (transp (-4))
           ,  Leaf (atMotifs [1,3]) (mlSD (-1))
           ,  Leaf (atMotifs [6]) (transp 1)
           ,  Leaf (atMotifs [7]) (transp(-3))
           ]
        ,  Node (atPeriods[4,5,6]) [
             Leaf (atMotifs [2,3]) (transp (-1))
           , Leaf (atMotifs [6,7]) (ct (1))
           ]
        ]
     ,  Node (atVoices [1])[
           Leaf (atPeriods[0,1,2,3,4,5,6,7,8]) (insert $ cvMotif2)
        ,  Leaf (atPeriods[3] .atMotifs [7]) (transp (-1))
        ,  Node (atPeriods [4,5,6]) [
              Leaf (atMotifs [0,1]) (transp (-4))
           ,  Leaf (atMotifs [2,3,4,5,6,7]) (transp (-5))
           ]
        ]
    , Leaf (atPeriods [8] . atMotifs[0]) (insert $ cMajor)
    ]

-- need to combine multiplegroups and act on them like 1 !!

cMajor = toGroup V [Note hn (C,4), Note hn (G,3)]
-- other version of "cellevevet"

cv2 :: MusicPT
cv2= Node (atPeriods [0,1,2,3,4,5,6,7]) [
         Node (atVoices [0] ) [ --
            Leaf (atMotifs [0,1,2,3,4,5,6,7]) (insert $ transp (-7) cvMotif)
         ,  Leaf (atMotifs [2,3]) (transp (-2))
         ,  Leaf (atMotifs [4,5]) (transp (-4))
         ,  Leaf (atMotifs [1,3]) (mlSD (-1))
         ,  Leaf (atMotifs [6]) (transp 1)
         ,  Leaf (atMotifs [7]) (transp(-3))
        ]
        ,  Node (atPeriods [4,5]) [
             Node (atVoices [0,1]) [
             Leaf (atMotifs [0,1,2,3,4,5,6,7]) (transp (-3))
             ]
           , Node (atVoices [1]) [
               Leaf (atMotifs [3,7]) (transp (-1))
             , Leaf (atMotifs [2]) (transp(1))
             ]
          ]
        , Leaf (atPeriods [8] . atMotifs[0]) (insert $ cMajor)
        ]

-}
chords :: OrientedTree (Primitive Pitch)
chords =
              Group H [
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (C,4)),
                  Val (Note hn (E,4)),
                  Val (Note hn (G,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ],
                Group V [
                  Val (Note hn (D,4)),
                  Val (Note hn (G,4)),
                  Val (Note hn (B,4))
                  ]
                ]


melody :: MusicTree
melody = Group H [
              Group H [
                  Group H [
                    Val (Note hn (C,4)),
                    Val (Note hn (E,4)),
                    Val (Note hn (G,4))
                    ],
                  Group H [
                    Val (Note hn (C,4)),
                    Val (Note hn (E,4)),
                    Val (Note hn (G,4))
                    ]
              ]
          ,   Group H [
                    Group H [
                      Val (Note hn (C,4)),
                      Val (Note hn (E,4)),
                      Val (Note hn (G,4))
                      ],
                    Group H [
                      Val (Note hn (D,4)),
                      Val (Note hn (E,4)),
                      Val (Note hn (G,4))
                      ]
                ]
          ]

-- MODIFIED YAN HAN: -----------------------------------------------------------

-- Used only to infer a skeletal 'OrientedTree' from a musical prefix tree.
data TreeShape =
  TAll TreeShape
  | TSome [TreeShape]
  | TLeaf
  deriving Show

makeStartingTree :: [TI] -> MusicTree
makeStartingTree tis =
  let slices        = map slc tis
      treeStructure = foldr addSlice TLeaf slices
  in  toDefaultOrientedTree treeStructure

toDefaultOrientedTree :: TreeShape -> MusicTree
toDefaultOrientedTree =
  go $ [Group V, Group H, Group H] ++ repeat (Group H) -- left is top
 where
  go (c : cs) TLeaf      = Group H [Val $ Rest sn]
  go (c : cs) (TAll  t ) = c [go cs t]
  go (c : cs) (TSome ts) = c . map (go cs) $ ts

extendList :: Int -> a -> [a] -> [a]
extendList n e xs | n <= length xs = xs
                  | otherwise      = xs ++ replicate (n - length xs) e

mapChoice :: [Int] -> (a -> a) -> [a] -> [a]
mapChoice idxs f as =
  zipWith (\a idx -> if idx `elem` idxs then f a else a) as [0 ..]

addSlice :: Slice -> TreeShape -> TreeShape
addSlice []         t          = t
addSlice (All : xs) TLeaf      = TAll (addSlice xs TLeaf)
addSlice (All : xs) (TAll  t ) = TAll (addSlice xs t)
addSlice (All : xs) (TSome ts) = TSome (map (addSlice xs) ts)
addSlice (Some is : xs) TLeaf =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) TLeaf))
addSlice (Some is : xs) (TAll t) =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) t))
addSlice (Some is : xs) (TSome ts) =
  TSome (mapChoice is (addSlice xs) (extendList (maximum is + 1) TLeaf ts))
