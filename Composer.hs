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
import qualified Data.List as L
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
-- should they add? i.e. atVoices[0,1] . atVoices[2] = atVoices [0,1,2]?
-- right now atVoices[0,1] . atVoices[2] = atVoices [0,1]

atPeriods  = atDepth 0
atPhrases = atDepth 1
atMeasures = atDepth 2

--crashes if lvl >= length slice (might fix with Maybe)
atLevel :: Int -> [Int] -> Slice -> Slice
atLevel lvl selection slice =
  let (first, second) = splitAt lvl (reverse slice)
  in reverse $ first ++ [Some selection] ++ tail second

atDepth :: Int -> [Int] -> Slice -> Slice
atDepth lvl selection slice =
  let (first, second) = splitAt lvl slice
  in first ++ [Some selection] ++ tail second

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

data TI = TI { slc :: Slice, gt :: GT}  -- Transformative Instruction


-- TRANSFORMATIVE INSTRUCTION 2 MUSIC TREE -------------------------------------
tis2Tree :: [TI] -> MusicTree
tis2Tree instructions = applyTIs instructions (makeStartingTree instructions)

applyTIs :: [TI] -> MusicTree -> MusicTree
applyTIs instructions startingTree =
  foldl (flip applyTI) startingTree instructions

applyTI :: TI -> MusicTree -> MusicTree
applyTI (TI slice gt) tree = applyGT slice gt tree

-- PREFIXTREE 2 TRANSFORMATIVE INSTRUCTIONS ------------------------------------

flattenSTs :: Slice -> [Slice -> Slice] -> Slice
flattenSTs levels sts = foldl(\acc f -> f acc) levels sts

tightFlattenSts :: [Slice -> Slice] -> Slice
tightFlattenSts sts = flattenSTs (getAlls sts) sts

getAlls sts = replicate ((getMaxDepth sts) + 1) All

toTI :: ([Slice -> Slice], GT) -> TI
toTI (sts, gtrans) = TI {slc = tightFlattenSts sts, gt = gtrans}

pt2TIs :: MusicPT -> [TI]
pt2TIs pt =
  let stss = getAllPaths pt
      maxLevels = getAlls $ concat stss
      gts = getAllValues $ fmap ($ maxLevels) pt
  in map (toTI) $ zip stss gts

pt2MT :: MusicPT -> MusicTree
pt2MT pt = tis2Tree $ pt2TIs pt

getMaxDepth :: [Slice -> Slice] -> Int
getMaxDepth sts = maximum $ map getDepth sts

-- a song cannot have more that 666 hierarchical levels, should be generalized
getDepth :: (Slice -> Slice) -> Int
getDepth sTrans = maximum $ L.findIndices (isSome) $ sTrans $ replicate (666) All

isSome (Some xs) = True
isSome _  = False

getSlices pt = map slc $ pt2TIs pt

test = applyTIs (pt2TIs lol) (pt2MT dt)
-- TESTING ZONE: ---------------------------------------------------------------


p tree = playDevS 6 $ tempo 0.80 $ (treeToMusic tree) --quick play

mkChord pitch mode dur = map (\p -> Note dur p) $ pitches $ getTriad pitch mode

mc p o m = insert $ toGroup V $ mkChord (p,o) m hn

cv_chords = Node (atPeriods [0,1,2,3,4,5,6,7]) [
              Node (atPeriods [0,1,2,3,7]) [
                Node (atMeasures [0,1]) [
                    Leaf (atPhrases [0]) (mc C 3 Major)
                ,   Leaf (atPhrases [1]) (mc A 2 Minor)
                ,   Leaf (atPhrases [2]) (mc F 2 Major)
                ,   Node (atPhrases [3]) [
                        Leaf (atMeasures [0]) (mc D 3 Minor)
                    ,   Leaf (atMeasures [1]) (mc G 2 Major)
                    ]
                ]
                , Leaf (atMeasures [0,1]) (toCV)
                , Leaf (atPhrases [0,1] . atMeasures [1] . atDepth 3 [0]) (mlSD (-1))
            ]
            , Node (atPeriods [4,5,6]) [
                Node (atMeasures [0,1]) [
                    Leaf (atPhrases [0]) (mc F 2 Major)
                ,   Leaf (atPhrases [2]) (mc A 2 Minor)
                ,   Leaf (atPhrases [3]) (mc A 2 Major)
                ,   Node (atPhrases [1]) [
                        Leaf (atMeasures [0]) (insert $ toGroup V [Note hn (E,2), Note hn (A,2), Note hn ((B,2))])
                    ,   Leaf (atMeasures [1]) (mc E 2 Major)
                    ]
                ]
              , Leaf (atMeasures [0,1]) (toCV)

              ]
            ]

dt = Node (atDepth 0 [0])[
        Leaf (atDepth 1 [0,1]) (mc C 3 Major)
     ,  Leaf (atDepth 1 [0]) (toCV)
     , Node (atDepth 1 [0]) [
        Leaf (atDepth 2 [1]) (transp 2)
        ]
    ]
lol = Node (atDepth 0 [0]) [
        Node (atDepth 1 [0]) [
            Leaf (atDepth 2 [0]) (transp 2)
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
{-
toCV2 :: MusicTree -> MusicTree
toCV2 (Group V notes) =
 let n = replaceDurations [sn,sn,sn] (fromGroup V) notes
     voice1 = [T.replaceDuration (sn) $ head notes]
     voice2 = [Group V $ map (replaceDuration (sn)) $ tail notes]
     f = concat . replicate 4
 in Group V [  Group H ( f voice1)
            ,  Group H ( (Rest sn :: Primitive Pitch) : f voice2)
            ]
-}
tes = [Val $ Note en (C,4), Val $ Note en (C,3)]


toVal = (\x -> Val x)

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
  go $ repeat (Group H) -- left is top
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
