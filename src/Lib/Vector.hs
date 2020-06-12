{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Vector where

import qualified Prelude
import           Feldspar
import           Feldspar.Run
import           Feldspar.Data.Vector

import           Lib.Control
import           Lib.Array

-- | Transform a 2D pull vector into a 1D in row-major order
flattenPull :: Pull2 a -> Pull a
flattenPull (Pull2 rows cols f) =
  Pull (rows * cols) $ \ix -> f (ix `div` cols) (ix `mod` cols)

-- | Inverse of flattenPull, given the number of rows
nestPull :: Data Length -> Pull a -> Pull2 a
nestPull segs (Pull len f) = Pull2 rows cols $ \row col -> f $ row * cols + col
 where
  rows = segs
  cols = len `div` segs

-- | Mutable 2D array to Pull2
arr2ToPull2 :: (Syntax a, MonadComp m) => Nest (Arr a) -> m (Pull2 a)
arr2ToPull2 arr = do
  iarr <- freezeArr $ unnest arr
  return $ nestPull (nestNumSegs arr) . toPull $ iarr

zipWith2 :: (a -> b -> c) -> Pull2 a -> Pull2 b -> Pull2 c
zipWith2 f a b =
  nestPull (numRows a) $ zipWith f (flattenPull a) (flattenPull b)

map2 :: (a -> b) -> Pull2 a -> Pull2 b
map2 f v = nestPull (numRows v) $ map f (flattenPull v)

sum2 :: (Syntax a, Num a) => Pull2 a -> a
sum2 = sum . flattenPull


-- | Writes a Pushy2 matrix to a 2D array, given a starting location
push2DStore
  :: (Pushy2 m vec a, MonadComp m, Syntax a)
  => Nest (Arr a)             -- ^ The 2d array to store the matrix in
  -> (Data Index, Data Index) -- ^ The starting location to store in
  -> vec                      -- ^ The matrix to store
  -> m ()
push2DStore loc (y, x) vec = do
  assert (y + (r - 1) < h && x + (c - 1) < w) "push2DStore: Index out of bounds"
  dumpPush2 v $ \i j elem -> setArr2 loc (y + i, x + j) elem
 where
  v      = toPush2 vec
  (r, c) = extent2 v
  (h, w) = extent2 loc

pPrintPush2
  :: forall a
   . (Formattable a, PrimType' a, Type a, Ord a, Integral a)
  => Push2 Run (Data a)
  -> Run ()
pPrintPush2 vec = dumpPush2 vec $ \i j elem -> do
  numLen <- countLen elem
  maxL   <- maxLen
  let lenDiff = maxL - numLen
  for (0 :: Data Int32, 1, Excl lenDiff) (printf " ")
  printf "%d  " elem
  if1 (j + 1 == numCols vec) $ printf "\n"

 where
  maxLen :: Run (Data Int32)
  maxLen = do
    rMaxLen <- initRef 0
    dumpPush2 vec $ \i j elem -> do
      maxLen  <- getRef rMaxLen
      thisLen <- countLen elem
      if1 (thisLen > maxLen) $ setRef rMaxLen thisLen
    getRef rMaxLen

  countLen :: Data a -> Run (Data Int32)
  countLen num = do
    curr  <- initRef num
    count <- initRef 0
      -- If the number is negative, add one to the counter (for the - sign)
      -- and negate it so the below algorithm works
    if1 (num < 0) do
      modifyRef curr negate
      modifyRef count (+1)

    while ((> 0) <$> getRef curr) do
      modifyRef curr (`div` 10)
      modifyRef count (+ 1)

    -- If the number is 0, it has length 1
    ifE (num == 0) (pure 1) (getRef count)


pDumpPull2
  :: (PrimType a, Integral a, Ord a, Formattable a)
  => Handle
  -> DPull2 a
  -> Run ()
pDumpPull2 h pull = dumpPush2 vec $ \i j elem -> do
  let leng    = numLen elem
      maxLen  = colLengths ! j
      lenDiff = i2n $ maxLen - leng
  for (0 :: Data Int32, 1, Excl lenDiff) (const $ fprintf h " ")
  fprintf h " %d" elem
  if1 (j + 1 == numCols vec) $ printf "\n"

 where
  vec = toPush2 pull
  colLengths =
    map (fold max 0) $ map (map numLen) $ exposeRows $ transpose pull

-- | Use this instad of pPrintPush2 if possible since this one aligns numbers better
pPrintPull2
  :: (PrimType a, Integral a, Ord a, Formattable a) => DPull2 a -> Run ()
pPrintPull2 = pDumpPull2 stdout

-- | Cut a Pull2 to specified width and height
-- | Does not check wether new dimentions are lower than the previous
cutPull2 :: Pull2 a -> (Data Length, Data Length) -> Pull2 a
cutPull2 (Pull2 h' w' f) (h, w) = Pull2 h w $ \i j -> f i j

-- | Ugly test for the length of an integral number which does not require
-- us to use string casting or the Run monad
numLen :: (Num a, Integral a, PrimType a) => Data a -> Data Word32
numLen num = guardVal (aNum < 999999) "numLen: number too big" (len + sgn)
  where
    sgn, aNum, len :: Data Word32
    sgn  = cond (num < 0) 1 0
    aNum = i2n $ cond (num < 0) (- num) (num)
    len  = (aNum == 0     ) ? 1 $
           (aNum < 10     ) ? 1 $
           (aNum < 100    ) ? 2 $
           (aNum < 1000   ) ? 3 $
           (aNum < 10000  ) ? 4 $
           (aNum < 100000 ) ? 5 $
           (aNum < 1000000) ? 6 $
                              7

pDumpArr2
  :: forall a
   . (Formattable a, PrimType' a, Type a, Ord a, Integral a)
  => Handle
  -> Arr2 a
  -> Run ()
pDumpArr2 h arr = do
  pull <- arr2ToPull2 arr
  pDumpPull2 h pull

pPrintArr2
  :: forall a
   . (Formattable a, PrimType' a, Type a, Ord a, Integral a)
  => Arr2 a
  -> Run ()
pPrintArr2 = pDumpArr2 stdout


printPush2
  :: (Formattable a, PrimType' a, Type a) => Push2 Run (Data a) -> Run ()
printPush2 vec =
  dumpPush2 vec
    $ (\i j elem -> do
        ifE (j + 1 == numCols vec) (printf "\n") (pure ())
        printf "(%d, %d): %d, " i j elem
      )

printPush :: (Formattable a, PrimType' a, Type a) => Push Run (Data a) -> Run ()
printPush vec =
  dumpPush vec $ \ix elem -> printf "element: %d index: %d \n" elem ix

printPush'
  :: (Formattable a, PrimType' a, Type a) => Push Run (Data a) -> Run ()
printPush' vec = do
  dumpPush vec $ \ix elem -> printf "%d " elem
  printf "\n"
