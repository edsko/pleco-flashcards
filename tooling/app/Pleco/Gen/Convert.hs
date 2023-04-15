-- | Monad for doing conversions
--
-- Intended for unqualified import
module Pleco.Gen.Convert (
    -- * Conversion monad
    Convert -- opaque
  , Error(..)
  , LineNo(..)
  , NoStats(..)
  , runConvert
  , isolateState
    -- * Standard conversion functions
  , numberLines
  , skipEmptyLines
  , stripHashComments
    -- * Re-exports
  , MonadError(..)
  , MonadState(..)
  , MonadWriter(..)
  , modify
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Writer (MonadWriter(..))
import Data.Bifunctor
import Data.Char (isSpace)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad for doing conversions
--
-- Supports
--
-- * Internal state @s@ (whatever is needed for the conversion)
-- * Statistics @w@, shown to the user after the conversion
-- * Located errors
newtype Convert s w a = Wrap {
      unwrap :: StateT s (ExceptT Error (Writer w)) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    , MonadError Error
    , MonadState s
    , MonadWriter w
    )

data Error = Error LineNo String

newtype LineNo = LineNo { getLineNo :: Word }
  deriving newtype (Enum)

-- | Useful for conversions that do not produce additional statistics
data NoStats = NoStats
  deriving (Show)

instance Monoid    NoStats where mempty = NoStats
instance Semigroup NoStats where _ <> _ = NoStats

runConvert :: Monoid w => Convert () w a -> (Either Error a, w)
runConvert =
      first (fmap fst)
    . runWriter
    . runExceptT
    . flip runStateT ()
    . unwrap

-- | Make state internal to the conversion
isolateState ::
     s             -- ^ Initial state
  -> (s -> a -> b) -- ^ Convert result using final state
  -> Convert s w a -> Convert () w b
isolateState s f =
      Wrap
    . lift
    . fmap (uncurry $ flip f)
    . flip runStateT s
    . unwrap

{-------------------------------------------------------------------------------
  Standard conversion functions
-------------------------------------------------------------------------------}

-- | Number each line (for error messages)
numberLines :: [String] -> [(LineNo, String)]
numberLines = zip [LineNo 1..]

-- | Skip empty lines
--
-- The type guarantees that this is done /after/ line numbering.
skipEmptyLines :: [(LineNo, String)] -> [(LineNo, String)]
skipEmptyLines = filter $ any (not . isSpace) . snd

-- | Strip everything after a hash (@#@)
stripHashComments :: String -> String
stripHashComments = takeWhile (/= '#')