{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Macroblock where

import qualified Prelude
import           Feldspar.Run            hiding ( liftRun )
import           Feldspar.Data.Option
import           Control.Monad.Reader
import           Feldspar.Data.Vector

import           VLC.VLC                       as VLC
import           Lib.DataTypes
import           Lib.Data
import           Lib.Util
import           Lib.Vector
import           Lib.Control
import           Lib.Files
import           Lib.Math
import           Lib.Array
import           Block

type PatternCodeFun = Data Index -> Data Word32

type PredMotionVector = (Data Int32, Data Word32, Data Int32, Data Word32)

type MotionVector = (Data Int32, Data Int32)

type MBType = (Data Bool, Data Bool, Data Bool, Data Bool, Data Bool)

toMBType :: (Data Int32, Data Int32, Data Int32, Data Int32, Data Int32) -> MBType
toMBType (a, b, c, d, e) = (i2b a, i2b b, i2b c, i2b d, i2b e)

mbIntra, mbPatt, mbMotionFwd :: MBType -> Data Bool
mbIntra     (_, _, _, _, i) = i
mbPatt      (_, _, _, p, _) = p
mbMotionFwd (_, f, _, _, _) = f


-- | Given a nullable code_block_pattern, and the variable macroblock_intra,
-- returns the function \i -> pattern_code[i]
patternCodeF :: Option (Data Word32) -> Data Bool -> PatternCodeFun
patternCodeF ocbp intra i = option noCbp (hasCbp i) ocbp
 where
  noCbp :: String -> Data Word32
  noCbp = const $ cond intra 1 0

  hasCbp :: Data Index -> Data Word32 -> Data Word32
  hasCbp i cbp = (i2b $ cbp .&. (1 .<<. (5 - (i2n i)))) ? 1 $
                                                  intra ? 1 $
                                                          0

readAddrIncr :: Decoder (Data Int32)
readAddrIncr = do
  -- Read macroblock stuffing
  while ((== 0xF) <$> peekBits 11) $ void $ nextBits 11 >> (printf "stuffing\n")
  ret :: DRef Int32 <- initNamedRef "addr_incr" 0

  loop do
    incr <- getVLC VLC.macroblock_address_increment
    -- If incr is an escape code
    iff (incr == -2)
      do
        modifyRef ret (+33)
      do
        modifyRef ret (+incr)
        break

  getRef ret


readMBlock :: Data Bool -> Decoder ()
readMBlock first = do
  r         <- mbRow
  c         <- mbCol
  addr_incr <- asksRef mbAddrIncr
  raddr     <- asks mbAddress
  addr      <- getRef raddr

  picnum    <- asksRef picNum

  logf "----[MB]--(%d, %d)---Pic: %d -----\n" r c picnum

  if1 (addr_incr == 0) $ do
    new_incr <- readAddrIncr
    if1 (new_incr < 1) ((printf "addr_incr negative") >> exit "cringe")
    setsRef mbAddrIncr (i2n new_incr)

    if1 (first) $ do
      --modifyRef raddr (+ (i2n new_incr))
      setsRef mbAddrIncr 1


  skipped <- (/= 1) <$> asksRef mbAddrIncr


  (pmv, mbType) <- ifE (not skipped)
                      do notSkipped1
                      do pure (some (0,0,0,0), (false,false,false,false,false))

    -- Reset previous motion vectors
  if1 (skipped || mbIntra mbType || not (mbMotionFwd mbType)) resetPrevMV

  mv <- ifE1 (skipped || (not $ mbIntra mbType)) $
               skippedNonIntra $ option (const (0,0,0,0)) id pmv

  if1 (not skipped) $ notSkipped2 (mbIntra mbType) (mbPatt mbType) mv

  if1 (skipped) do
    decodeMBlock (\_ -> 0) false (some (0, 0))

  setsRef prevMbAddress addr
  if1 (mbIntra mbType) (setsRef pastIntraAddr $ i2n addr)

  addr_incr_ref <- asks mbAddrIncr
  modifyRef (addr_incr_ref) (pred)
  modifyRef raddr  (+1)

 where
   notSkipped1 :: Decoder ((Option PredMotionVector), MBType)
   notSkipped1 = do

     mbType@(mbQuant, mbMotionFwd, mbMotionBck, mbPatt, mbIntra) <-
                                                   toMBType <$> getVlcPicType

     if1 (mbMotionBck) $ exit "mbMotionBck not supported"

     if1 (mbQuant) $ do
       q <- nextBits 5
       setsRef qScale q

     -- Get motion vectors forward
     ifE (mbMotionFwd)
       do fw <- readMotionFwd
          pure (some fw           , mbType)
       do pure (none "No fwVector", mbType)


   skippedNonIntra :: PredMotionVector -> Decoder MotionVector
   skippedNonIntra pv = do
     -- reset DC predictors
     setsRef dcYPast  128
     setsRef dcCrPast 128
     setsRef dcCbPast 128

     decodeMotionFwd pv


   notSkipped2 :: Data Bool -> Data Bool  -> Option MotionVector -> Decoder ()
   notSkipped2 intra patt mv = do
     -- Get pattern code
     cbp <- ifE1 patt (getVLC VLC.coded_block_pattern)
     let patternCode = patternCodeF (i2n <$> cbp) intra

     decodeMBlock patternCode intra mv

   resetPrevMV :: Decoder ()
   resetPrevMV = do
     setsRef reconRFwdPrev 0
     setsRef reconDFwdPrev 0

   getVlcPicType = do
     pic_type <- asksRef picType
     ifE (pic_type == 1)   (getVLC VLC.macroblock_type_code_i) $
       ifE (pic_type == 2) (getVLC VLC.macroblock_type_code_p)
                           (exit "bad picture type! \n")


readMotionFwd :: Decoder PredMotionVector
readMotionFwd = do
  fwd_f_code <- asksRef fwdFCode
  let fwdRSize = i2n fwd_f_code - 1
      fwd_f    = (1 :: Data Word32) .<<. (fwdRSize)

  motionHFwdCode <- getVLC motion_vector_codes
  motionHFwdR    <- ifE (fwd_f /= 1 && motionHFwdCode /= 0)
                        (nextBits fwdRSize)
                        (pure 0)

  motionVFwdCode <- getVLC motion_vector_codes
  motionVFwdR    <- ifE (fwd_f /= 1 && motionVFwdCode /= 0)
                        (nextBits fwdRSize)
                        (pure 0)
  pure (motionHFwdCode, motionHFwdR, motionVFwdCode, motionVFwdR)


decodeMotionFwd :: PredMotionVector -> Decoder MotionVector
decodeMotionFwd (motionHFwdCode, motionHFwdR, motionVFwdCode, motionVFwdR) = do

  fwd_f_code <- asksRef fwdFCode
  let fwdRSize = (i2n fwd_f_code) - 1
      fwd_f    = (1 :: Data Word32) .<<. (fwdRSize)

  compHorFwdR <- ifE ((fwd_f == 1) || (motionHFwdCode == 0))
                     (pure 0)
                     (pure $ fwd_f - 1 - motionHFwdR)

  compVerFwdR <- ifE ((fwd_f == 1) || (motionVFwdCode == 0))
                     (pure 0)
                     (pure $ fwd_f - 1 - motionVFwdR)

  prev_v           <- asksRef reconDFwdPrev
  prev_h           <- asksRef reconRFwdPrev

  rightLB@(rl, rb) <- littleBig compHorFwdR fwd_f motionHFwdCode
  downLB@( dl, db) <- littleBig compVerFwdR fwd_f motionVFwdCode

  reconRight       <- reconVec fwd_f reconRFwdPrev rightLB
  reconLeft        <- reconVec fwd_f reconDFwdPrev downLB

  pure (reconRight, reconLeft)

 where
   littleBig :: Data Word32 -> Data Word32 -> Data Int32
             -> Decoder (Data Int32, Data Int32)
   littleBig compFwdR fwd_f motionFwdCode = do
     let littleTmp = motionFwdCode * i2n fwd_f

     ifE (littleTmp == 0)
       do pure (0, 0)
       do ifE (littleTmp > 0)
             do let little = littleTmp - i2n compFwdR
                    big    = little - (32 * i2n fwd_f)
                pure (little, big)
             do let little = littleTmp + i2n compFwdR
                    big    = little + (32 * i2n fwd_f)
                pure (little, big)

   reconVec
     :: Data Word32
     -> (DecoderEnv -> DRef Int32)
     -> (Data Int32, Data Int32)
     -> Decoder (Data Int32)
   reconVec fwd_f recon_prev_sel (little, big) = do
     fullPelFwdVec <- asksRef fullPelFwdVec
     recon_prev    <- asksRef recon_prev_sel

     let max     = (16 * i2n fwd_f) - 1
         min     = (-16) * i2n fwd_f
         new_vec = recon_prev + little

     recon <- ifE ((new_vec <= max) && (new_vec >= min))
                  (pure $ recon_prev + little)
                  (pure $ recon_prev + big)

     setsRef recon_prev_sel recon

     ifE (fullPelFwdVec == 1) (pure $ recon .<<. 1) (pure recon)



decodeMBlock :: PatternCodeFun -> Data Bool -> Option MotionVector
             -> Decoder ()
decodeMBlock patternCode intra motionFwd =
  for (0 :: Data Word32, 1, Excl 6) \b -> do

    decoded :: Arr2 Int32 <- constArr2 [Prelude.replicate 8 0 | _ <- [0..7]]
    if1 (patternCode b == 1) do
      block   <- readBlock b intra

      iff (intra) (decodeIBlock b block decoded)
                  (decodePBlock block decoded)

    pull <- arr2ToPull2 decoded

    iff (intra)
      do
        storeBlock b (idctW8 pull)
      do
        let fVec = option (const (0,0)) id motionFwd
        motion     <- decodeMotionVectors fVec b
        motionPull <- arr2ToPull2 motion
        -- If blockData exists, combine it with motion data
        iff (patternCode b == 1)
          do
            storeBlock b $ zipWith2 addClamp motionPull (idctI32 pull)
          do
            storeBlock b motionPull

-- | Add two Word8 while avoiding an overflow and
-- | clamp the result to 255
addClamp :: Data Word8 -> Data Int32 -> Data Word8
addClamp a b = (a32 + b > 255) ? 255 $
               (a32 + b < 0)   ? 0   $
                                (a + (i2n b))
  where a32 :: Data Int32 = i2n a

