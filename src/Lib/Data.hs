{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE FlexibleInstances #-}


module Lib.Data where

import qualified Prelude
import           Feldspar.Run            hiding ( liftRun )
import           Lib.DataTypes
import           Lib.Files
import           Lib.Util
import           Language.Embedded.Expression

data VLC a = Node (VLC a) (VLC a)
           | Leaf a
           | End
  deriving (Show)

getVLC :: (Syntax a) => VLC a -> Decoder a
getVLC End        = exit "getVLC reached `End`"
getVLC (Leaf v  ) = return v
getVLC (Node i o) = do
  rbit <- nextBit
  ifE (rbit == 0) (getVLC o) (getVLC i)

instance Show (Data Int32) where
  show s = "(" Prelude.++ show (evalExp s) Prelude.++ ")"

instance Read (Data Int32) where
  readsPrec _ s = [(constExp w, rest)]
    where [(w, rest)] = readsPrec 0 s :: [(Int32, String)]
