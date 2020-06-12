-- | Defines some custom control flow structures
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Lib.Control where

import           Feldspar
import           Feldspar.Data.Option


-- | Two nested for-loops
for2
  :: (Integral n, PrimType n, MonadComp m)
  => IxRange (Data n)
  -> IxRange (Data n)
  -> (Data n -> Data n -> m ())
  -> m ()
for2 r1 r2 b = for r1 (for r2 . b)

-- | an 'if' statement without an 'else' branch
if1 :: MonadComp m => Data Bool -> m () -> m ()
if1 cond a = iff cond a (pure ())

-- | an 'ifE' statement without an else brance, returning 'nothing'
-- if the condition evaluates to false
ifE1 :: (MonadComp m, Syntax a) => Data Bool -> m a -> m (Option a)
ifE1 cond a = ifE cond (some <$> a) (pure $ none "Nothing")

-- | Allows to choose between multiple monadic actions. Will perform only the
-- first action that evaluates to true
switchME
  :: (MonadComp m, Syntax a)
  => [(Data Bool, m a)]  -- ^ Cases (Bool, action)
  -> m a                 -- ^ Default case
  -> m a                 -- ^ Result
switchME []            def = def
switchME ((b, m) : cs) def = ifE b m (switchME cs def)

switchM
  :: MonadComp m
  => [(Data Bool, m ())]  -- ^ Cases (Bool, action)
  -> m ()                 -- ^ Default case
  -> m ()                 -- ^ Result
switchM []            def = def
switchM ((b, m) : cs) def = iff b m (switchM cs def)

-- | A version of switchM which does noting in the default case
switchM1
  :: MonadComp m
  => [(Data Bool, m ())]  -- ^ Cases (Bool, action)
  -> m ()                 -- ^ Result
switchM1 cs = switchM cs (pure ())

-- | An infinite loop `while (true)`
loop :: (MonadComp m) => m () -> m ()
loop = while (pure true)

-- | does not peform the given computation. Useful for quickly commenting
-- out a do block
ignore :: Monad m => m a -> m ()
ignore m = pure ()

