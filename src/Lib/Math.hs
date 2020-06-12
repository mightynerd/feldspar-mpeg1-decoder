{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}


module Lib.Math where

import qualified Prelude
import           Prelude                        ( fromIntegral )
import           Feldspar
import           Feldspar.Data.Vector
import           Feldspar.Run
import           Lib.Vector
import           Lib.Array
import           Lib.Util
import           DctRef                         ( tstDctInput )


-- | 8x8 IDCT without clamping
idct :: DPull2 Int32 -> DPull2 Int32
idct v = map2 round $ a `matMul` f `matMul` transpose a
 where
  f = map2 i2n v :: DPull2 Double
  η u = cond (u == 0) (1 / sqrt 8) (1 / 2) :: Data Double
  a = Pull2 8 8 $ \x u -> η u * cos (i2n ((2 * x + 1) * u) * π / 16)


-- | 8x8 IDCT with clamping to Word8
idctW8 :: DPull2 Int32 -> DPull2 Word8
idctW8 = map2 clampByte . idct

-- | 8x8 IDCT with clamping to [-256, 255]
idctI32 :: DPull2 Int32 -> DPull2 Int32
idctI32 = map2 clamp . idct where clamp v = min 255 (max (-256) v)

clampByte :: (Integral a, Ord a, PrimType a) => Data a -> Data Word8
clampByte v = i2n $ min 255 (max 0 v)


testIDCT :: Run ()
testIDCT = do
  arr :: Arr2 Int32 <- constArr2
    $ (Prelude.map . Prelude.map $ fromIntegral) tstDctInput
  ourPull <- arr2ToPull2 arr
  pPrintPush2 $ toPush2 $ idct ourPull

ceil :: (RealFrac a, Num b, PrimType a, PrimType b) => Data a -> Data b
ceil f = round $ f + 0.5

-- | Integer division rounded towards the nearest integer
-- | (`//` in the MPEG-1 specification)
rdiv :: (Integral a, PrimType a) => Data a -> Data a -> Data a
rdiv a b = (a + (b `quot` 2)) `quot` b


idctTest :: Run ()
idctTest = do
  let vec   = Pull2 8 8 $ \i j -> i2n $ i * j
      idctd = idct vec

  a :: DArr Int32 <- newArr 64
  let n = nest 8 8 a

  push2DStore n (0, 0) idctd

