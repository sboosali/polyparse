--------------------------------------------------

{-# LANGUAGE CPP #-}

--------------------------------------------------

#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(x,y,z1,z2) 0
#endif
-- NOTE `ghc-7.10` introduced `MIN_VERSION_GLASGOW_HASKELL`.

--------------------------------------------------

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 0
#endif

--------------------------------------------------

-- | Compatibility module.
--
-- Exports 'Control.Monad' and (when present) 'Control.Monad.Fail'.
-- 
-- NOTE: `ghc-8.0.*` bundles `base-4.9.0.0`, which introduces `Control.Monad.Fail`.
--

module Text.ParserCombinators.Poly.Compat

#if MIN_VERSION_base(4,9,0)
  ( module Text.ParserCombinators.Poly.Compat
  , module Control.Monad
  , module Control.Monad.Fail
#else
  ( module Text.ParserCombinators.Poly.Compat
  , module Control.Monad
#endif
  ) where

--------------------------------------------------

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad      hiding (fail)
#else
import Control.Monad
#endif

--------------------------------------------------

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
#endif

--------------------------------------------------

#if __GLASGOW_HASKELL__
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

--------------------------------------------------