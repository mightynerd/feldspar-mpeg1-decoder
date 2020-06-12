{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.DataTypes
  ( Decoder
  , FileHandle(..)
  , DecoderEnv(..)
  , liftRun
  , setsRef
  , asksRef
  )
where

import qualified Prelude
import           Control.Monad.State.Class
import           Control.Monad.State
import           Control.Monad.Reader
import           Feldspar.Run            hiding ( liftRun )
import           Feldspar.Data.Array

instance (MonadComp m) => MonadComp (ReaderT r m) where
  liftComp = lift . liftComp
  iff c t f = do
    env <- ask
    lift $ iff c (runReaderT t env) (runReaderT f env)
  for rng body = do
    env <- ask
    lift $ for rng $ \n -> runReaderT (body n) env
  while c body = do
    env <- ask
    lift $ while (runReaderT c env) (runReaderT body env)


------------------------------------------------------------------------------

data FileHandle = FH
  {
    fhl         :: Object             -- ^ The file handle
  , currentBit  :: Ref (Data Int32)   -- ^ Reference to the current bit read state
  , currentWord :: Ref (Data Word32)  -- ^ Reference to the last read word
  }

data DecoderEnv = DecoderEnv
  {
    fileHandle    :: FileHandle
  , writeHandle   :: FileHandle
  --
  , iqMatrix      :: Nest (DArr Int32)
  , niqMatrix     :: Nest (DArr Int32)
  , scanMatrix    :: Nest (DArr Int32)
  --
  , hSize         :: DRef Word32
  , vSize         :: DRef Word32
  --
  , picType       :: DRef Word8
  , qScale        :: DRef Word32
  , mbAddress     :: DRef Word32
  , prevMbAddress :: DRef Word32
  , mbAddrIncr    :: DRef Word32
  , sliceVPos     :: DRef Word32
  , fullPelFwdVec :: DRef Word32
  , fwdFCode      :: DRef Word32
  , pastIntraAddr :: DRef Int32
  , dcYPast       :: DRef Int32
  , dcCbPast      :: DRef Int32
  , dcCrPast      :: DRef Int32
  --
  , reconRFwdPrev :: DRef Int32
  , reconDFwdPrev :: DRef Int32
  --
  , lumBuff       :: Nest (DArr Word8)
  , cbBuff        :: Nest (DArr Word8)
  , crBuff        :: Nest (DArr Word8)

  , lumBuffPrev   :: Nest (DArr Word8)
  , cbBuffPrev    :: Nest (DArr Word8)
  , crBuffPrev    :: Nest (DArr Word8)

  , picNum        :: DRef Word32
  }

type Decoder = ReaderT DecoderEnv Run

liftRun :: Run a -> ReaderT DecoderEnv Run a
liftRun = lift

setsRef :: Syntax a => (DecoderEnv -> Ref a) -> a -> Decoder ()
setsRef sel val = do
  ref <- asks sel
  setRef ref val

asksRef :: Syntax a => (DecoderEnv -> Ref a) -> Decoder a
asksRef sel = asks sel >>= getRef

instance (a ~ ()) => PrintfType (Decoder a)
  where
  fprf h form args = liftRun (fprf h form args)
