{-# LANGUAGE CPP #-}

------------------------------
#ifndef MIN_VERSION_GLASGOW_HASKELL
#define MIN_VERSION_GLASGOW_HASKELL(x,y,z1,z2) 0
#endif
-- NOTE `ghc-7.10` introduced `MIN_VERSION_GLASGOW_HASKELL`.
------------------------------

module Text.ParserCombinators.Poly.Text
  ( -- * The Parser datatype
    Parser(P)
  , Result(..)
  , runParser
    -- ** Basic parsers
  , next
  , eof
  , satisfy
  , onFail
    -- ** Derived parsers (but implemented more efficiently)
  , manySatisfy
  , many1Satisfy
    -- ** Re-parsing
  , reparse
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where


import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Control.Applicative

import Text.ParserCombinators.Poly.Compat
import Prelude hiding (fail)

--------------------------------------------------------------------------------

-- | This @Parser@ datatype is a specialised parsing monad with error
--   reporting.  Whereas the standard version can be used for arbitrary
--   token types, this version is specialised to Text input only.
newtype Parser a = P (Text -> Result Text a)

-- | Apply a parser to an input token sequence.
runParser :: Parser a -> Text -> (Either String a, Text)
runParser (P p) = resultToEither . p

instance Functor Parser where
    fmap f (P p) = P (fmap f . p)

------------------------------
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 800)
------------------------------

instance Applicative Parser where

   pure  = pureParser

   {-# INLINE pure #-}

   (<*>) = ap

   {-# INLINE (<*>) #-}

instance Monad Parser where

   (>>=) = bindParser

   {-# INLINE (>>=) #-}

instance MonadFail Parser where

  fail = failParser

  {-# INLINE fail #-}

------------------------------
#else
------------------------------

instance Applicative Parser where
    pure = pureParser
    pf <*> px = do { f <- pf; x <- px; pure (f x) }

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 610
    p  <*  q  = p `discard` q
#endif

instance Monad Parser where
    return = pureParser
    fail   = failParser
    (>>=)  = bindParser

------------------------------
#endif
------------------------------

pureParser :: a -> Parser a
pureParser x = P (\ts -> Success ts x)

failParser :: String -> Parser a
failParser e = P (\ts -> Failure ts e)

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser (P f) g = P (continue . f)
  where
  continue (Success ts x)             = let (P g') = g x in g' ts
  continue (Committed r)              = Committed (continue r)
  continue (Failure ts e)             = Failure ts e

------------------------------------------------------------------------

instance Alternative Parser where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance Commitment Parser where
    commit (P p)         = P (Committed . squash . p)
      where
        squash (Committed r) = squash r
        squash r             = r
    (P p) `adjustErr` f  = P (adjust . p)
      where
        adjust (Failure z e) = Failure z (f e)
        adjust (Committed r) = Committed (adjust r)
        adjust  good         = good

    oneOf' = accum []
      where accum errs [] =
                fail ("failed to parse any of the possible choices:\n"
                            ++indent 2 (concatMap showErr (reverse errs)))
            accum errs ((e,P p):ps) =
                P (\ts-> case p ts of
                           Failure _ err ->
                                       let (P p') = accum ((e,err):errs) ps
                                       in p' ts
                           r@(Success _ _)    -> r
                           r@(Committed _)    -> r )
            showErr (name,err) = name++":\n"++indent 2 err

instance PolyParse Parser

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next :: Parser Char
next = P (\bs-> case T.uncons bs of
                Nothing       -> Failure bs "Ran out of input (EOF)"
                Just (c, bs') -> Success bs' c )

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof :: Parser ()
eof = P (\bs -> if T.null bs
                then Success bs ()
                else Failure bs "Expected end of input (EOF)" )

-- | Return the next token if it satisfies the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do { x <- next
               ; if f x then return x else fail "Parse.satisfy: failed"
               }

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail :: Parser a -> Parser a -> Parser a
(P p) `onFail` (P q) = P (\ts-> continue ts $ p ts)
  where continue ts (Failure _ _) = q ts
    --  continue _  (Committed r) = r    -- no, remain Committed
        continue _  r             = r

------------------------------------------------------------------------

-- | @manySatisfy p@ is a more efficient fused version of @many (satisfy p)@
manySatisfy :: (Char->Bool) -> Parser Text
manySatisfy f = P (\bs-> let (pre,suf) = T.span f bs in Success suf pre)

-- | @many1Satisfy p@ is a more efficient fused version of @many1 (satisfy p)@
many1Satisfy :: (Char->Bool) -> Parser Text
many1Satisfy f = do x <- manySatisfy f
                    if T.null x then fail "Parse.many1Satisfy: failed"
                                else return x

------------------------------------------------------------------------

-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: Text -> Parser ()
reparse ts  = P (\inp-> Success (ts `T.append` inp) ())

------------------------------------------------------------------------
