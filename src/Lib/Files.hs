{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Files
  ( ropen
  , rclose
  , rreset
  , wopen
  , withropen
  , withwopen
  , checkFileOpened
  , getAddr
  , writeByteArr
  , readWord32
  , readWord32R
  , readWord8R
  , bswap
  , FileHandle
  , splitBits
  , nextBits
  , nextBit
  , peekBits
  , byteAlign
  , lookFor
  , lookForR
  , eof
  )
where

import           Prelude                        ( (!!) )
import qualified Prelude
import           Feldspar.Run            hiding ( liftRun )
import           Feldspar.Representation        ( arrLength )

import           Lib.DataTypes                  ( Decoder
                                                , DecoderEnv(..)
                                                , FileHandle(..)
                                                , liftRun
                                                )
import           Lib.Control
import           Lib.Util

import           Control.Monad.Reader

checkFileOpened :: FileHandle -> Run (Data Bool)
checkFileOpened (FH o _ _) = do
  addInclude "<stdio.h>"
  addDefinition checkFH
  ok <- callFun "checkFH" [objArg o] :: Run (Data Int32)
  pure (i2b ok)
 where
  checkFH = [cedecl|
      int checkFH(typename FILE *fh) {
            return fh ? 1 : 0;
      }
		|]

-- | Create a file handle for writing
wopen :: String -> Run FileHandle
wopen name = do
  o    <- newNamedObject "f" "FILE" True
  bit  <- initNamedRef "currentBit" 0
  word <- initNamedRef "currentWord" 0
  callProcAssign o "fopen" [strArg name, strArg "w"]
  pure $ FH o bit word

-- | Create a file handle for reading
ropen :: String -> Run FileHandle
ropen name = do
  o    <- newNamedObject "f" "FILE" True
  bit  <- initNamedRef "currentBit" 0
  word <- initNamedRef "currentWord" 0
  callProcAssign o "fopen" [strArg name, strArg "r"]
  pure $ FH o bit word

rclose :: FileHandle -> Run ()
rclose (FH handle _ _) = callProc "fclose" [objArg handle]

withropen :: String -> (FileHandle -> Run ()) -> Run ()
withropen filename f = do
  file <- ropen filename
  f file
  rclose file

withwopen :: String -> (FileHandle -> Run ()) -> Run ()
withwopen filename f = do
  file <- wopen filename
  f file
  rclose file

-- | Gets the current address in a file
getAddr :: FileHandle -> Run (Data Int32)
getAddr (FH handle _ _) = callFun "ftell" [objArg handle]

-- | Return whether a file handle has reached the end of the file
eof :: FileHandle -> Run (Data Bool)
eof (FH handle _ _) = do
  res :: Data Int32 <- callFun "feof" [objArg handle]
  return $ res /= 0

-- | rewinds the file to the beginning and resets currenWord and currentBit
rreset :: FileHandle -> Run ()
rreset (FH fh b w) = do
  callProc "fseek" [ objArg fh
                   , valArg (0 :: Data Int32)
                   , valArg (0 :: Data Int32)
                   ]
  setRef b 0
  setRef w 0

-- | rewinds the file one Word32 and sets currentWord and currentBit
-- | according to the arguments
rback :: FileHandle -> Data Int32 -> Data Word32 -> Run ()
rback (FH fh b w) b' w' = do
  callProc "fseek" [ objArg fh
                   , valArg (-4 :: Data Int32)
                   , valArg (1  :: Data Int32)
                   ]
  setRef b b'
  setRef w w'


readWord32 :: Decoder (Data Word32)
readWord32 = do
  fh@(FH _ b w) <- asks fileHandle
  word          <- liftRun $ readData fh 4 >>= bswap
  setRef w word
  setRef b 0
  pure word

readWord32R :: FileHandle -> Run (Data Word32)
readWord32R fh = readData fh 4 >>= bswap

readWord8R :: FileHandle -> Run (Data Word8)
readWord8R fh = readData fh 1

readData :: (PrimType' a, Type a) => FileHandle -> (Data Int8) -> Run (Data a)
readData fh@(FH handle b w) typesize = do
  end <- eof fh
  if1 end (exitR "End of file reached, exiting...")

  ref :: Ref (Data a) <- newNamedRef "read_target"
  callProc "fread"
    [ refArg ref
    , valArg typesize
    , valArg (1 :: Data Length)
    , objArg handle
    ]

  setRef b 0
  getRef ref

writeByteArr :: FileHandle -> DArr Word8 -> Data Length -> Run ()
writeByteArr (FH h _ _) arr l = do
  callProc "fwrite"
    [ arrArg arr
    , valArg (1 :: Data Int8)
    , valArg l
    , objArg h
    ]

{- | Splits a word into a list of words, given a list of starting bits
 - and ranges. For example, to split the word 0xFA660700 into FA6 and 07,
 - call splitBits [(20,12),(8,8)] 0xFA660700 since FA6 starts at bit 20 and
 - has a width of 12 bits, and 07 starts at bit 8 and has a width of 8 bits.
-}
splitBits :: [(Data Int32, Data Int32)] -> Data Word32 -> [Data Word32]
splitBits ranges word =
  [ lsbs width $ word .>>. start | (start, width) <- ranges ]

testBit :: Data Word32 -> Data Int32 -> Data Word32
testBit w i = cond ((splitBits [(i, 1)] w) !! 0 == 1) 1 0

byteAlign :: Decoder ()
byteAlign = do
  (FH _ rbit rword) <- asks fileHandle
  bit               <- getRef rbit
  let numForward = (-bit) `mod` 8
  if1 (numForward /= 0) $ void $ nextBits numForward

-- | Like nextBits but it resets currentBit and currentWord
peekBits :: Data Int32 -> Decoder (Data Word32)
peekBits n = do
  assert (n <= 32 && n > 0) "Invalid bit count for peekBits"
  fh@(FH _ rbit rword) <- asks fileHandle
  bit                  <- getRef rbit
  word                 <- getRef rword

  -- TODO convert into nextBits
  res <- initNamedRef "nb_res" 0
  for (n - 1 :: Data Int32, -1, Incl 0) $ \i -> do
    next <- nextBit
    modifyRef res (+ (next .<<. i))

  bit' <- getRef rbit
  iff (bit' <= bit)
    do
      liftRun $ rback fh bit word
    do
      setRef rword word
      setRef rbit bit

  getRef res

nextBit :: Decoder (Data Word32)
nextBit = do
  fh@(FH _ rbit rword) <- asks fileHandle
  bit                  <- getRef rbit
  if1 (bit > 31) $ void $ readWord32
  word <- getRef rword
  modifyRef rbit (+ 1)
  pure $ testBit word (31 - bit)

nextBits :: Data Int32 -> Decoder (Data Word32)
nextBits n = do
  assert (n <= 32 && n > 0) "Invalid bit count for nextBits"
  res <- initNamedRef "nb_res" 0
  for (n - 1 :: Data Int32, -1, Incl 0) $ \i -> do
    next <- nextBit
    modifyRef res (+ (next .<<. i))
  getRef res


beToLe :: Data Word32 -> Data Word32
beToLe be =
  (be .<<. 24)
    + ((be .&. 0x0000FF00) .<<. 8)
    + ((be .&. 0x00FF0000) .>>. 8)
    + (be .>>. 24)

bswap :: Data Word32 -> Run (Data Word32)
bswap w = do
  addInclude "<byteswap.h>"
  callFun "bswap_32" [valArg w]

readBytes :: Data Length -> Run (DArr Int8)
readBytes len = do
  fh@(FH fhandle _ _) <- ropen "file"
  arr                 <- newArr len
  callProc "fread"
    [ arrArg arr
    , valArg (1 :: Data Int8)
    , valArg len
    , objArg fhandle
    ]
  rclose fh
  pure arr

runRead :: Run ()
runRead = do
  arr <- readBytes 5
  for (0, 1, Excl $ arrLength arr) $ \i -> do
    elem <- getArr arr i
    printf "%x \n" elem

-- | Read until (including) the given predicate is true for the current Word32
lookFor :: (Data Word32 -> Data Bool) -> Decoder (Data Word32)
lookFor pred = do
  lookFor <- newNamedRef "lookFor"

  while (pure true) do
    word <- readWord32
    if1 (pred word) $ setRef lookFor word >> break

  getRef lookFor

lookForR :: FileHandle -> (Data Word32 -> Data Bool) -> Run (Data Word32)
lookForR fh pred = do
  lookFor <- newNamedRef "lookFor"

  while (pure true) do
    word <- readWord32R fh
    if1 (pred word) $ setRef lookFor word >> break

  getRef lookFor
