{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import qualified Prelude

import           System.Environment
import           System.Exit
import           System.Directory


import           Feldspar.Run            hiding ( liftRun )
import           Feldspar.Data.Vector

import           Lib.Util
import           Lib.Files
import           Lib.DataTypes
import           Lib.Data
import           Lib.Array
import           Lib.Vector
import           Lib.Control
import qualified Matrices.Matrices             as Matrices

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Macroblock
import           Block


main = do
  putStrLn "feldspar-mpeg1-decoder\nGenerating code..."
  args <- getArgs
  if (Prelude.length args) Prelude.< 2
    then decode_
    else do
      let file = args Prelude.!! 0
          cmd  = args Prelude.!! 1
      isfile <- doesFileExist file

      unless isfile do
        putStrLn $ file Prelude.++ " does not exist"
        exitWith (ExitFailure 1)

      unless (cmd `Prelude.elem` ["run", "compile"]) do
        putStrLn $ "Invalid command: " Prelude.++ cmd
        exitWith (ExitFailure 1)

      case cmd of
        "run"      -> decode file
        "compile"  -> icompile $ mainRun file

decode :: String -> IO ()
decode fileName = run $ mainRun fileName

decode_ :: IO ()
decode_ = decode "../video"

-- | Constants
constSequenceHeaderCode = 0x000001B3
constGOPStartCode       = 0x000001B8
constPicStartCode       = 0x00000100
constMBlockStuffing     = 0xF
constExtensionStartCode = 0x000001B5
constUserDataStartCode  = 0x000001B2
constSequenceEndCode    = 0x000001B7


-- | A bit of a hack to compensate for the fact that we need to know the
-- resolution of the file to allocate our frame buffers before we start
-- decoding. After doing this, we must rewind the file handle and reset our
-- FileHandle state so we can start decoding "from the beginning"
getResolution :: FileHandle -> Run (Data Word32, Data Word32)
getResolution fh = do
  lookForR fh (== constSequenceHeaderCode)
  res <- readWord32R fh
  let [w, h] = splitBits [(20, 12), (8, 12)] res
  pure (w, h)

decodeSequence :: Decoder ()
decodeSequence = do
  printf "\n"
  printf "Decoding sequence \n"

  width  <- nextBits 12
  height <- nextBits 12

  setsRef hSize width
  setsRef vSize height

  aspect_ratio <- nextBits 4
  frame_rate   <- nextBits 4
  bit_rate     <- nextBits 18
  marker_bit   <- nextBit

  assert (marker_bit == 1) "Sequence marker_bit != 1"

  buffer_size        <- nextBits 10
  param_flag         <- nextBit
  load_intra_qmatrix <- nextBit

  if1 (load_intra_qmatrix == 1) (readQuantizerMatrix iqMatrix)

  printf "aspect_ratio: %d, frame_rate: %d\n" aspect_ratio frame_rate
  printf "bit_rate: %dk\n" ((bit_rate * 400) `div` 1000)
  printf "load_intra_qmatrix: %d\n" load_intra_qmatrix

  load_nintra_qmatrix <- nextBit
  if1 (load_nintra_qmatrix == 1) (readQuantizerMatrix niqMatrix)
  printf "load_nintra_qmatrix: %d\n" load_nintra_qmatrix

  nextStartCode
  skipExtension >> skipUserData

  loop do
    readGOP
    peek <- peekBits 32
    if1 (peek /= constGOPStartCode) (break)


readGOP :: Decoder ()
readGOP = do
  gopStartCode <- nextBits 32
  assert (gopStartCode == constGOPStartCode) "GOP start code not found!"

  (TC h m s p) <- decodeTimeCode <$> nextBits 25
  closed       <- nextBit
  broken       <- nextBit

  nextStartCode
  skipExtension >> skipUserData
  printf "GopTimeCode : %02d:%02d:%02d,%d \n" h m s p

  loop do
    readPic
    next <- peekBits 32
    if1 (next /= constPicStartCode) (break)

data TimeCode = TC { tcHours :: Data Word32
                   , tcMins  :: Data Word32
                   , tcSecs  :: Data Word32
                   , tcPics  :: Data Word32
                   }

decodeTimeCode :: Data Word32 -> TimeCode
decodeTimeCode w = TC h m s p
  where [p, s, m, h] = splitBits [(1, 5), (6, 6), (13, 6), (19, 6)] w

readPic :: Decoder ()
readPic = do
  picnumref <- asks picNum
  picNum    <- getRef picnumref
  printf "------------------[Picture %d]------------------\n" picNum

  picStart <- nextBits 32
  tempRef  <- nextBits 10
  typ      <- nextBits 3

  printf "Picture type: %d, temporal reference: %d\n" typ tempRef
  assert (typ == 1 || typ == 2) "B- and D-frames not supported"

  setsRef picType $ i2n typ
  vbvDelay <- nextBits 16

  if1 (typ == 2 || typ == 3) do
    full_pel_fwd_vec <- nextBit
    forward_f_code   <- nextBits 3
    assert (forward_f_code /= 0) "forward_f_code cannot be 0"
    setsRef fwdFCode forward_f_code
    setsRef fullPelFwdVec full_pel_fwd_vec

  if1 (typ == 3) (exit "B-frames not supported")

  while ((== 1) <$> peekBits 1) do
    -- extra_information_picture
    void nextBit
    void $ nextBits 8

  extra_bit <- nextBit
  assert (extra_bit == 0) "extra_bit_picture must be 0"

  nextStartCode

  skipExtension >> skipUserData
  setsRef mbAddress 0
  loop do
    readSlice
    peek <- peekBits 32
    if1 (not $ isSliceStartCode peek) (break)

  modifyRef picnumref (+1)
  writePicture
  swapBuffers

swapBuffers :: Decoder ()
swapBuffers =
  forM_ [(lumBuff, lumBuffPrev), (cbBuff, cbBuffPrev), (crBuff, crBuffPrev)]
    \(curr, prev) -> do
      pBuff <- unnest <$> asks prev
      cBuff <- unnest <$> asks curr
      copyArr pBuff cBuff

clearFrontBuffers :: Decoder ()
clearFrontBuffers = do
  forM_ [lumBuff, cbBuff, crBuff] $ \buffSel -> do
    buff <- asks buffSel
    liftRun $ clearArr2 buff 1

clearArr2 :: (PrimType a) => Nest (DArr a) -> Data Word32 -> Run ()
clearArr2 arr dSize =
  callProc "memset" [ arrArg (unnest arr)
                    , valArg (0 :: Data Int32)
                    , valArg (numRows arr * numCols arr * dSize)
                    ]

-- | Possibly faster version of swapBuffers which swaps pointers in C
-- | Does currently not work

--swapBuffers :: Decoder ()
--swapBuffers = do
--  liftRun $ addDefinition swap_ptrs
--
--  forM_ [(lumBuff, lumBuffPrev), (cbBuff, cbBuffPrev), (crBuff, crBuffPrev)]
--    \(currRef, prevRef) -> do
--      curr <- unnest <$> asks currRef
--      prev <- unnest <$> asks prevRef
--      liftRun $ callProc "swap_ptrs" [ addr . addr $ arrArg curr
--                                     , addr . addr $ arrArg prev ]

--   where
--     swap_ptrs = [cedecl|
--       void swap_ptrs(typename uint8_t **p1, typename uint8_t **p2) {
--           char *temp = *p1;
--           *p1 = *p2;
--           *p2 = temp;
--       }
--       |]

readSlice :: Decoder ()
readSlice = do
  startCode <- nextBits 32
  assert (isSliceStartCode startCode) "Illegal slice start code"
  let [vPos] = splitBits [(0, 8)] startCode
  setsRef sliceVPos vPos

  -- reset the previous macroblock address to (slice_v_position-1)*mb_width-1
  mb_width <- mbWidth
  setsRef prevMbAddress $ (vPos - 1) * mb_width - 1

  -- reset block decoding variables
  setsRef pastIntraAddr (-2)
  setsRef dcYPast  1024
  setsRef dcCrPast 1024
  setsRef dcCbPast 1024

  -- reset motion vectors
  setsRef reconRFwdPrev 0
  setsRef reconDFwdPrev 0

  qscale <- nextBits 5
  assert (qscale > 0) "quantizer scale cannot be 0"
  setsRef qScale qscale

  while ((==1) <$> (peekBits 1)) $
    void $ nextBits 9

  extra_bit_slice <- nextBit
  assert (extra_bit_slice == 0) "extra_bit_slice must be 0"

  -- Reset mbAddrIncr
  setsRef mbAddrIncr 0

  -- First MBlock
  readMBlock true

  loop do
    peek <- peekBits 23
    if1 (peek == 0) (break)
    readMBlock false

  nextStartCode


skipUserData :: Decoder ()
skipUserData = skipExtraData constExtensionStartCode

skipExtension :: Decoder ()
skipExtension = skipExtraData constUserDataStartCode

skipExtraData :: Data Word32 -> Decoder ()
skipExtraData code = do
  condExt <- (== code) <$> peekBits 32
  if1 (condExt) do
    void $ nextBits 32
    while ((/= 1) <$> peekBits 32) (void $ nextBits 8)
    nextStartCode

isSliceStartCode :: Data Word32 -> Data Bool
isSliceStartCode w = w >= 0x00000101 && w <= 0x000001AF

readQuantizerMatrix :: (DecoderEnv -> Arr2 Int32) -> Decoder ()
readQuantizerMatrix envf = do
  arr <- asks envf
  for (0 :: Data Index, 1, Excl 64) $ \i -> do
    val <- nextBits 8
    setArr (unnest arr) i (i2n val)
    printf "QMatrix[%d] = %d\n" i val

-- | Writes a picture to file
writePicture :: Decoder ()
writePicture = do
  f <- asks writeHandle
  w <- asksRef hSize
  h <- asksRef vSize
  forM_ [(lumBuff, 1), (cbBuff, 2), (crBuff, 2)] \(sel, scale) -> do
    buff <- asks sel
    pull <- arr2ToPull2 buff
    push2DStore buff  (0, 0) $ cutPull2 pull (h `div` scale, w `div` scale)

    liftRun $ writeByteArr f (unnest buff) ((w*h) `div` (scale * scale))

nextStartCode :: Decoder ()
nextStartCode = do
  byteAlign
  loop do
    peek <- peekBits 24
    iff (peek /= 1) (void $ nextBits 8) (break)

decodeVideoFile :: Decoder ()
decodeVideoFile = do
  lookFor (== constSequenceHeaderCode)
  readWord32 -- Don't know why this is needed
  loop do
    decodeSequence
    peek <- nextBits 32
    if1 (peek /= constSequenceHeaderCode) (break)


mainRun :: String -> Run ()
mainRun fileName = withropen fileName $ \fh -> do
  opened <- checkFileOpened fh
  if1 (not opened) (exitR $ "failed to open file " Prelude.++ fileName)

  -- Before we can do anything we need to find out the resolution of the file
  (w, h) <- getResolution fh
  rreset fh

  -- Frame buffer resolution must be a multiple of 16 (8 for chrominance)
  let w16 = 16 * ((w `div` 16) + cond ((w `mod` 16) == 0) 0 1)
      h16 = 16 * ((h `div` 16) + cond ((h `mod` 16) == 0) 0 1)

  printf "Video resolution: (%d, %d)\n"  w   h
  printf "Frame buffer size: (%d, %d)\n" w16 h16

  -- Create the default intra-quant-matrix
  iqMatrix :: Arr2 Int32 <- constNamedArr2 "iq_matrix"
                                           Matrices.intra_quantizer_matrix

  -- Create the default non-intra-quant-matrix (all values are 16)
  niqMatrix :: Arr2 Int32 <- constNamedArr2
    "niq_matrix"
    [ Prelude.replicate 8 16 | _ <- [0 .. 7] ]

  -- Create the zig-zag scan order matrix
  scanMatrix :: Arr2 Int32 <- constNamedArr2 "scan" Matrices.scan

  -- Create frame buffers
  lumBuff :: Arr2 Word8 <- nest h16 w16 <$> newNamedArr "lum_buff" (w16 * h16)
  cbBuff :: Arr2 Word8 <- nest (h16 `div` 2) (w16 `div` 2)
    <$> newNamedArr "cb_buff" ((w16 * h16) `div` 4)
  crBuff :: Arr2 Word8 <- nest (h16 `div` 2) (w16 `div` 2)
    <$> newNamedArr "cr_buff" ((w16 * h16) `div` 4)

  lumBuffPrev :: Arr2 Word8 <- nest h16 w16
    <$> newNamedArr "lum_buff_prev" (w16 * h16)
  cbBuffPrev :: Arr2 Word8 <- nest (h16 `div` 2) (w16 `div` 2)
    <$> newNamedArr "cb_buff_prev" ((w16 * h16) `div` 4)
  crBuffPrev :: Arr2 Word8 <- nest (h16 `div` 2) (w16 `div` 2)
    <$> newNamedArr "cr_buff_prev" ((w16 * h16) `div` 4)

  -- Initiate various variables
  hSize         <- initNamedRef "h_size" 0
  vSize         <- initNamedRef "v_size" 0

  picType       <- initNamedRef "pic_type" 0
  qScale        <- initNamedRef "q_scale" 1
  mbAddress     <- initNamedRef "mb_address" 0
  prevMbAddress <- initNamedRef "prev_mb_address" 0
  mbAddrIncr    <- initNamedRef "mb_addr_incr" 0
  sliceVPos     <- initNamedRef "slice_v_pos" 0
  fullPelFwdVec <- initNamedRef "full_pel_fwd_vec" 0
  fwdFCode      <- initNamedRef "forward_f_code" 0
  pastIntraAddr <- initNamedRef "past_intra_addr" ((-2) :: Data Int32)
  dcYPast       <- initNamedRef "dc_y_past" 1024
  dcCrPast      <- initNamedRef "dc_cr_past" 1024
  dcCbPast      <- initNamedRef "dc_cb_past" 1024

  reconRFwdPrev <- initNamedRef "recon_r_fwd_pred" 0
  reconDFwdPrev <- initNamedRef "recon_d_fwd_pred" 0

  picNum        <- initNamedRef "picture_number" 0


  wfh           <- wopen "../v.yuv"

  let initEnv = DecoderEnv { fileHandle    = fh
                           , writeHandle   = wfh
                           --
                           , iqMatrix      = iqMatrix
                           , niqMatrix     = niqMatrix
                           , scanMatrix    = scanMatrix
                           --
                           , hSize         = hSize
                           , vSize         = vSize
                           --
                           , picType       = picType
                           , qScale        = qScale
                           , mbAddress     = mbAddress
                           , prevMbAddress = prevMbAddress
                           , mbAddrIncr    = mbAddrIncr
                           , sliceVPos     = sliceVPos
                           , fullPelFwdVec = fullPelFwdVec
                           , fwdFCode      = fwdFCode
                           , pastIntraAddr = pastIntraAddr
                           , dcYPast       = dcYPast
                           , dcCrPast      = dcCrPast
                           , dcCbPast      = dcCbPast
                           --
                           , reconRFwdPrev = reconRFwdPrev
                           , reconDFwdPrev = reconDFwdPrev
                           --
                           , lumBuff       = lumBuff
                           , cbBuff        = cbBuff
                           , crBuff        = crBuff
                           --
                           , lumBuffPrev   = lumBuffPrev
                           , cbBuffPrev    = cbBuffPrev
                           , crBuffPrev    = crBuffPrev
                           --
                           , picNum        = picNum
                           }

  void $ runReaderT decodeVideoFile initEnv
