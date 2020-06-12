{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Block where

import qualified Prelude
import           Feldspar.Run            hiding ( liftRun )
import           Control.Monad.Reader
import           Feldspar.Data.Vector
import           Feldspar.Data.Option

import           VLC.VLC                       as VLC
import           Lib.DataTypes
import           Lib.Data
import           Lib.Util
import           Lib.Vector
import           Lib.Control
import           Lib.Files
import           Lib.Array
import           Lib.Math                       ( rdiv )

mbWidth :: Decoder (Data Word32)
mbWidth = do
  h_size <- asksRef hSize
  pure $ (h_size + 15) `div` 16

mbHeight :: Decoder (Data Word32)
mbHeight = do
  v_size <- asksRef vSize
  pure $ (v_size + 15) `div` 16

mbRow :: Decoder (Data Word32)
mbRow = div <$> asksRef mbAddress <*> mbWidth

mbCol :: Decoder (Data Word32)
mbCol = mod <$> asksRef mbAddress <*> mbWidth

readEscape :: Decoder ((Data Word32), (Data Int32))
readEscape = do
  run   :: Data Word32 <- nextBits 6
  first :: Data Int8   <- i2n <$> nextBits 8
  ifE (first /= -128 && first /= 0)
    do
      -- The level is only 8 bits
      return (run, i2n first)
    do
      -- The level is 16 bits
      second :: Data Int32 <- i2n <$> nextBits 8
      let res = cond (first == -128) (-256 + second) (second)
      return (run, res)

{- | Given a block type, stores the block at the appropriate position in
- the appropriate frame buffer.
- FOR DEFAULT PATTERN_CODE, the following applies (see MPEG1 standard p. 30)
-  * BlockType 0 -> upper left luminance
-  * BlockType 1 -> upper right luminance
-  * BlockType 2 -> lower left luminance
-  * BlockType 3 -> lower right luminance
-  * BlockType 4 -> Cb block
-  * BlockType 5 -> Cr block
-}
storeBlock :: BlockType -> DPull2 Word8 -> Decoder ()
storeBlock bt pull =
  switchM1
    [ (bt == 0, storeAt 0 0 16 =<< asks lumBuff)
    , (bt == 1, storeAt 0 1 16 =<< asks lumBuff)
    , (bt == 2, storeAt 1 0 16 =<< asks lumBuff)
    , (bt == 3, storeAt 1 1 16 =<< asks lumBuff)
    , (bt == 4, storeAt 0 0 8  =<< asks cbBuff)
    , (bt == 5, storeAt 0 0 8  =<< asks crBuff)
    ]
  where
    storeAt :: Data Word32
            -> Data Word32
            -> Data Word32
            -> Nest (DArr Word8)
            -> Decoder ()
    storeAt b_y b_x mbsize arr2 = do
      mb_row <- mbRow
      mb_col <- mbCol

      let frame_x = mbsize * mb_col + b_x * 8
          frame_y = mbsize * mb_row + b_y * 8

      -- Write the block unless it is out of bounds. This is corréct since pixels outiside
      -- of the specified boudns should be discarded according the MPEG1 standard
      let skip = (mbsize `div` 2) - 1
      iff (not (frame_y + skip >= (numRows arr2) || (frame_x + skip) >= (numCols arr2)))
        (push2DStore arr2 (frame_y, frame_x) pull)
        (printf "Skipping mbsize: %d, (%d, %d)\n" mbsize frame_x frame_y)

type BlockType = Data Word32

isYFstBlock, isYBlock, isCbBlock, isCrBlock :: BlockType -> Data Bool
isYFstBlock b = b == 0
isYBlock    b = b > 0 && b < 4
isCbBlock   b = b == 4
isCrBlock   b = b == 5


decodeMotionVectors :: (Data Int32, Data Int32) -> BlockType -> Decoder (Arr2 Word8)
decodeMotionVectors (reconRight, reconDown) bType = do
  pixels :: Arr2 Word8 <- constArr2 [ Prelude.replicate 8 0 | _ <- [0 .. 7] ]

  switchM1
    [ (bType == 0, go 0 0 16 pixels =<< asks lumBuffPrev)
    , (bType == 1, go 0 1 16 pixels =<< asks lumBuffPrev)
    , (bType == 2, go 1 0 16 pixels =<< asks lumBuffPrev)
    , (bType == 3, go 1 1 16 pixels =<< asks lumBuffPrev)
    , (bType == 4, go 0 0 8 pixels  =<< asks cbBuffPrev)
    , (bType == 5, go 0 0 8 pixels  =<< asks crBuffPrev)
    ]

  return pixels

  where
    go :: Data Word32
       -> Data Word32
       -> Data Word32
       -> Arr2 Word8
       -> Nest (DArr Word8)
       -> Decoder ()
    go b_y b_x mbsize pixels prev = do
      let recon_right = cond (bType < 4) (reconRight) (reconRight `quot` 2)
          recon_down  = cond (bType < 4) (reconDown)  (reconDown  `quot` 2)
          right_for      = recon_right .>>. 1
          down_for       = recon_down  .>>. 1
          right_half_for = recon_right - (2 * (right_for))
          down_half_for  = recon_down  - (2 * (down_for))
          range          = (0 :: Data Int32, 1, Excl 8)

      mb_row <- mbRow
      mb_col <- mbCol

      let col = i2n $ mbsize * mb_col + b_x * 8
          row = i2n $ mbsize * mb_row + b_y * 8

      for2 range range $ \m n -> do
        if1 (right_half_for == 0 && down_half_for == 0) $ do
          prevVal <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + row, i2n $  m + right_for + col) "prevVal (first)"
          setArr2 pixels (i2n $ n, i2n $ m) (i2n prevVal)

        if1 (right_half_for /= 0 && down_half_for == 0) $ do
          prevVal1 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + row, i2n $ m + right_for + col) "prevVal1 (2nd)"
          prevVal2 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + row, i2n $ m + right_for + 1 + col) "prevVal2 (2nd)"
          setArr2 pixels (i2n $ n, i2n $ m) (i2n $ (prevVal1 + prevVal2) `rdiv` 2)

        if1 (right_half_for == 0 && down_half_for /= 0) $ do
          prevVal1 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + row, i2n $ m + right_for + col) "prevVal1 (3rd)"
          prevVal2 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + 1 + row, i2n $ m + right_for + col) "prevVal2 (3rd)"
          setArr2 pixels (i2n $ n, i2n $ m) (i2n $ (prevVal1 + prevVal2) `rdiv` 2)

        if1 (right_half_for /= 0 && down_half_for /= 0) $ do
          prevVal1 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + row, i2n $ m + right_for + col) "prevVal1 (4th)"
          prevVal2 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + row, i2n $ m + right_for + 1 + col) "prevVal2 (4th)"
          prevVal3 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + 1 + row, i2n $ m + right_for + col) "prevVal3 (4th)"
          prevVal4 <- liftRun $ msgGetArr2 prev (i2n $ n + down_for + 1 + row, i2n $ m + right_for + 1 + col) "prevVal4 (4th)"
          setArr2 pixels (i2n $ n, i2n $ m) (i2n $ (prevVal1 + prevVal2 + prevVal3 + prevVal4) `rdiv` 4)


decodePBlock :: DArr Int32 -> Arr2 Int32 -> Decoder ()
decodePBlock b arr = do
  mbAddr    <- asksRef mbAddress
  pastIAddr <- asksRef pastIntraAddr

  go arr

 where
  go arr = do
    scan       <- asks scanMatrix
    quantScale <- i2n <$> asksRef qScale
    qMat       <- asks niqMatrix

    let range = (0 :: Data Word32, 1, Excl 8)
    for2 range range $ \m n -> do
      i  <- i2n <$> getArr2 scan (m, n)
      zz <- getArr b i
      q  <- getArr2 qMat (m, n)

      let elem   = (((2 * zz) + (signum zz)) * quantScale * q) `quot` 16
          elem'  = cond (even elem) (elem - (signum elem)) elem
          capped = min (max elem' (-2048)) 2047
          zeroed = cond (zz == 0) 0 capped

      setArr2 arr (m, n) zeroed


decodeIBlock :: BlockType -> DArr Int32 -> Arr2 Int32 -> Decoder ()
decodeIBlock bType b arr = do
  res <- firstPart arr

  switchM1
    [ (isYFstBlock bType, sndPart res =<< asks dcYPast)
    , (isYBlock bType   , sndPartY res)
    , (isCbBlock bType  , sndPart res =<< asks dcCbPast)
    , (isCrBlock bType  , sndPart res =<< asks dcCrPast)
    ]

  where

    firstPart :: Arr2 Int32 -> Decoder (Arr2 Int32)
    firstPart arr = do
      scan              <- asks scanMatrix
      quantScale        <- i2n <$> asksRef qScale
      qMat              <- asks iqMatrix

      let range = (0 :: Data Word32, 1, Excl 8)
      for2 range range $ \m n -> do
        i  <- i2n <$> getArr2 scan (m, n)
        zz <- getArr b i
        iq <- getArr2 qMat (m, n)

        let elem   = (2 * zz * quantScale * iq) `quot` 16
            elem'  = cond (even elem) (elem - (signum elem)) elem
            capped = min (max elem' (-2048)) 2047

        setArr2 arr (m, n) capped

      setArr2 arr (0, 0) =<< (8 *) <$> getArr b 0
      pure arr

    sndPart :: Arr2 Int32 -> DRef Int32 -> Decoder ()
    sndPart arr dcPastRef = do
      mbAddr    <- asksRef mbAddress
      pastIAddr <- asksRef pastIntraAddr
      dcPast    <- getRef  dcPastRef
      rec0      <- getArr2 arr (0, 0)

      let rec0' = cond ((i2n mbAddr) - pastIAddr > 1)
                    (rec0 + (128*8))
                    (dcPast + rec0)

      setArr2 arr (0, 0) rec0'
      setRef dcPastRef rec0'


    sndPartY :: Arr2 Int32 -> Decoder ()
    sndPartY arr = do
      yPast <- asksRef dcYPast
      setArr2 arr (0, 0) =<< (yPast + ) <$> ((8 *) <$> getArr b 0)
      setsRef dcYPast    =<< getArr2 arr (0, 0)


even n = n .&. 1 == 0

pred n = n - 1


readBlock :: Data Word32 -> Data Bool -> Decoder (DArr Int32)
readBlock i intra = do
  arr :: DArr Int32 <- constArr (Prelude.replicate (8 * 8) 0)

  firstCoeff <- ifE intra (some <$> intraFst) nonIntraFst
  if1 (isSome firstCoeff) $ setCoeffs (fromSome firstCoeff) arr

  return arr

  where
    setCoeffs :: (Data Word32, Data Int32) -> DArr Int32 -> Decoder ()
    setCoeffs (run, dctFst) arr = do
      iRef <- initNamedRef "dct_zz_index" (0 :: Data Index)
      setRef iRef run
      setArr arr run dctFst

      -- Write rest of coefficients
      loop do
        (r, l) <- getVLC VLC.dct_coeff_next
        if1 (r == -1) break  -- end_of_block
        (run, level) <- getRunLength r l
        -- Update index according to p. 31 (i = i + run + 1)
        modifyRef iRef (+ (run + 1))
        i <- getRef iRef

        setArr arr i level

    intraFst = ifE (i < 4) (getIntraFst VLC.dct_dc_size_luminance  )
                           (getIntraFst VLC.dct_dc_size_chrominance)

    getIntraFst :: VLC (Data Int32) -> Decoder (Data Word32, Data Int32)
    getIntraFst vlc = do
      size <- getVLC vlc
      ifE (size /= 0) do d <- nextBits size
                         pure (0, dctFirstIntraCoeff size d)
                      do pure (0, 0)

    nonIntraFst :: Decoder (Option (Data Word32, Data Int32))
    nonIntraFst = do
      (r, l) <- getVLC dct_coeff_first
      some <$> getRunLength r l


    getRunLength r l =
      ifE (r == -2) do readEscape
                    do sign <- nextBit
                       let l' = cond (sign == 0) (l) (-l)
                       return (i2n r, l')


-- | Returns the first dct coefficient for luminance blocks according to 2.4.3.7
dctFirstIntraCoeff :: Data Int32 -> Data Word32 -> Data Int32
dctFirstIntraCoeff size diff' =
  let diff = i2n diff'
  in  cond (i2b (diff .&. (1 .<<. (size - 1))))
           (diff)
           ((-1 .<<. size) .|. (diff + 1))
