{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Util where

import           Feldspar
import           Feldspar.Run            hiding ( liftRun )
import           Feldspar.Data.Option

import qualified Language.Embedded.Imperative  as Imp
import qualified Language.Embedded.Imperative.CMD
                                               as Imp

import           Lib.DataTypes


-- | Enable / disable this to turn logging on or off
traceLvl :: Data Bool
traceLvl = false

-- | Compiler flag for floating-point operations and optimization
compFlags = def { externalFlagsPost = ["-lm"] }
-- compFlags = def {externalFlagsPre = ["-O3"], externalFlagsPost = ["-lm"]}

-- | Version of runCompiled supporting floating-point operations
runCompiledF :: MonadRun m => m a -> IO ()
runCompiledF = runCompiled' def compFlags

run :: MonadRun m => m a -> IO ()
run = runCompiledF

continue :: (Monad m) => m ()
continue = pure ()

exit :: Syntax a => String -> Decoder a
exit s = liftRun $ exitR s

exitR :: Syntax a => String -> Run a
exitR s = do
  addInclude "<stdlib.h>"
  printf $ "Fatal error: `" Prelude.++ s Prelude.++ "`\n"
  callProc "exit" [valArg (1 :: Data Int32)]
  pure example

isSome :: Option a -> Data Bool
isSome = option (const false) (const true)

class LogfType r
  where
    lprf :: Handle -> String -> [Imp.PrintfArg Data] -> r

instance (a ~ ()) => LogfType (Run a)
  where
  lprf h form args = iff traceLvl (printf "gay" >> fprf h form args) (pure ())

instance (a ~ ()) => LogfType (Decoder a)
  where
  lprf h form args = liftRun (lprf h form args)

instance (Formattable a, PrimType a, LogfType r) => LogfType (Data a -> r)
  where
  lprf h form as a = lprf h form (Imp.PrintfArg a : as)

flogf :: LogfType r => Handle -> String -> r
flogf h format = lprf h format []

logf :: LogfType r => String -> r
logf = flogf Imp.stdout
