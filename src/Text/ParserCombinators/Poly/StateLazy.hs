{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module Text.ParserCombinators.Poly.StateLazy
  ( -- * The Parser datatype
    Parser(P)    -- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)    -- internal to the parser monad
  , runParser    -- :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
    -- ** Basic parsers
  , next    -- :: Parser s t t
  , eof        -- :: Parser s t ()
  , satisfy    -- :: (t->Bool) -> Parser s t t
  , onFail      -- :: Parser s t a -> Parser s t a -> Parser s t a
  , manyFinally    -- :: Parser s t a -> Parser s t z -> Parser s t [a]
    -- ** State-handling
  , stUpdate    -- :: (s->s) -> Parser s t ()
  , stQuery     -- :: (s->a) -> Parser s t a
  , stGet       -- :: Parser s t s
    -- ** Re-parsing
  , reparse    -- :: [t] -> Parser s t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where


import Text.ParserCombinators.Poly.Base hiding (manyFinally)
import Text.ParserCombinators.Poly.Result
import qualified Text.ParserCombinators.Poly.StateParser as P
import Control.Applicative

import Text.ParserCombinators.Poly.Compat
import Prelude hiding (fail)

------------------------------------------------------------------------

-- | The only differences between a State and a StateLazy parser are the
--   instance of Applicative, and the type (and implementation) of runParser.
--   We therefore need to /newtype/ the original Parser type, to allow it
--   to have a different instance.

newtype Parser s t a = P (P.Parser s t a)


------------------------------
#ifdef __GLASGOW_HASKELL__

    deriving ( Functor
             , Monad
#if MIN_VERSION_base(4,9,0)
             , MonadFail
#endif
             , Commitment
             )

------------------------------
#else
------------------------------

instance Functor (Parser t) where
    fmap f (P p) = P (fmap f p)

instance Monad (Parser t) where
    return x    = P (pure x)
    fail e      = P (fail e)
    (P f) >>= g = P (f >>= (\(P g') -> g') . g)

instance Commitment (Parser t) where
    commit (P p)   = P (commit p)
    (P p) `adjustErr` f  = P (p `adjustErr` f)

#endif
------------------------------

instance Applicative (Parser s t) where

    pure x = P (pure x)

    --   Apply a parsed function to a parsed value.  This version
    --   is strict in the result of the function parser, but
    --   lazy in the result of the argument parser.  (Argument laziness is
    --   the distinctive feature over other implementations.)

    (P (P.P pf)) <*> px = P (P.P go)

      where
        go s = continue . pf s

        continue (Success (z,s) f) = let (x,s',z') = runParser px s z
                                     in Success (z',s') (f x)
        continue (Committed r)     = Committed (continue r)
        continue (Failure zs e)    = Failure zs e

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 610
    (<*) = discard
    {-# INLINE (<*) #-}
#endif

-- | Apply a parser to an input token sequence.
runParser :: Parser s t a -> s -> [t] -> (a, s, [t])
runParser (P (P.P p)) = \s -> fromResult . p s
  where
    fromResult :: Result (z,s) a -> (a, s, z)
    fromResult (Success (z,s) a)  =  (a, s, z)
    fromResult (Failure   _   e)  =  throwE e
    fromResult (Committed r)      =  fromResult r

------------------------------

instance Alternative (Parser s t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser s t)

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next    ::  Parser s t t
next    = P P.next

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof     :: Parser s t ()
eof     = P P.eof

-- | Return the next token if it satisfies the given predicate.
satisfy :: (t->Bool) -> Parser s t t
satisfy = P . P.satisfy

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail  :: Parser s t a -> Parser s t a -> Parser s t a
onFail (P a) (P b) = P (a `P.onFail` b)

-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse :: [t] -> Parser s t ()
reparse = P . P.reparse

------------------------------------------------------------------------
-- State handling

-- | Update the internal state.
stUpdate   :: (s->s) -> Parser s t ()
stUpdate f  = P (P.stUpdate f)

-- | Query the internal state.
stQuery    :: (s->a) -> Parser s t a
stQuery f   = P (P.stQuery f)

-- | Deliver the entire internal state.
stGet      :: Parser s t s
stGet       = P (P.stGet)

------------------------------------------------------------------------


manyFinally :: Parser s t a -> Parser s t z -> Parser s t [a]
{-
manyFinally pp@(P p) pt@(P t) = P (\s ts -> item s ts (p s ts))
    where
      item _ _  (Success ts s x) = success ts s x
      item s ts (Failure _ _ e)  = terminate (t s ts)
      item s ts (Committed r)    = Committed (within r)

      success ts s x =
            let (tail,s',ts') = runParser (manyFinally pp pt) s ts
            in Success ts' s' (x:tail)

      terminate (Success ts s _) = Success ts s []
      terminate (Failure ts s e) = Failure ts s e
      terminate (Committed r)    = Committed (terminate r)

      within (Success ts s x)    = success ts s x
      within (Failure ts s e)    = Failure ts s e
      within (Committed r)       = within r
-}

manyFinally p z =
    (do x <- p; return (x:) `apply` manyFinally p z)
      `onFail`
    (do z; return [])
      `onFail`
    oneOf' [ ("item in sequence",    (do p; return []))
           , ("sequence terminator", (do z; return [])) ]

------------------------------------------------------------------------
