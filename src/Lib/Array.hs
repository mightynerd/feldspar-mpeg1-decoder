{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}


module Lib.Array where

import qualified Prelude
import           Prelude                        ( fromIntegral
                                                , maximum
                                                )
import           Feldspar
import           Feldspar.Run
import           Feldspar.Data.Vector
import           Lib.Util

-- | 2D array
type Arr2 a = Nest (DArr a)

constNamedArr2
  :: forall a m
   . (MonadComp m, Num (Internal a), PrimType (Internal a))
  => String
  -> [[Internal a]]
  -> m (Nest (Arr a))
constNamedArr2 name mat = nest h w <$> constNamedArr name (Prelude.concat padded)
 where
  pad :: [Internal a] -> [Internal a]
  pad lst = lst ++ Prelude.replicate (w' - length lst) 0

  h  = fromIntegral $ length mat
  w' = maximum $ map length mat
  w  = fromIntegral w'

  padded :: [[Internal a]]
  padded = map pad mat

  length = Prelude.length
  map    = Prelude.map
  (++)   = (Prelude.++)

constArr2
  :: (MonadComp m, Num (Internal a), PrimType (Internal a))
  => [[Internal a]]
  -> m (Nest (Arr a))
constArr2 = constNamedArr2 "a2"

getArr2
  :: (Syntax a, MonadComp m) => Nest (Arr a) -> (Data Index, Data Index) -> m a
getArr2 arr (y, x) = getArr (unnest arr) (y * numCols arr + x)

setArr2
  :: (Syntax a, MonadComp m)
  => Nest (Arr a)
  -> (Data Index, Data Index)
  -> a
  -> m ()
setArr2 arr (y, x) v = setArr (unnest arr) (y * numCols arr + x) v

printArr :: DArr Int32 -> Run ()
printArr arr = do
  for (0 :: Data Word32, 1, Excl (length arr))
      (\i -> getArr arr i >>= (\v -> printf "(%d, %d) " i v))
  printf "\n"

-- | GerArr functions with verbose error messanges if out of bounds
msgGetArr :: (Syntax a) => Arr a -> Data Index -> String -> Run a
msgGetArr arr ix name =
  ifE (ix < length arr)
    do getArr arr ix
    do printf ("getArr " Prelude.++ name Prelude.++
               ": index out of bounds:\n     Trying to get index %d" Prelude.++
                " in array of length %d\n")
                ix (length arr)
       exitR ""


msgGetArr2 :: Syntax a => Nest (Arr a) -> (Data Index, Data Index) -> String
            -> Run a
msgGetArr2 arr (row, col) name =
  ifE (col < numCols arr && row < numRows arr)
    do getArr2 arr (row, col)
    do printf ("getArr2 " Prelude.++ name Prelude.++
               ": index out of bounds:\n     Trying to get index (%d, %d)" Prelude.++
                " in array of dimensions (%d, %d)\n")
                col row (numCols arr) (numRows arr)
       exitR ""
