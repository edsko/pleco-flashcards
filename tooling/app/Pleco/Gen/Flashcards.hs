-- | Definition of the Pleco flashcards format
--
-- Intended for qualified import.
module Pleco.Gen.Flashcards (
    -- * Definition
    Flashcards(..)
  , Category(..)
  , Flashcard(..)
    -- * Assembly
  , listUnder
  , group
    -- * Queries
  , numCategories
  , numCards
    -- * Serialization
  , serialize
  ) where

import Data.Bifunctor
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Flashcards = Flashcards {
      getFlashcards :: [(Category, [Flashcard])]
    }

-- | Category
--
-- NOTE: Pleco only allows 3 levels, but we do not enforce that here.
newtype Category = Category [String]
  deriving (Eq, Ord)

-- | Individual flash card
--
-- The Pleco format also allows flashcards to contain pinyin pronunciation and
-- definitions, but we (currently) do not support these here.
data Flashcard = Flashcard Char

below :: Category -> Category -> Category
below (Category main) (Category sub) = Category (main ++ sub)

{-------------------------------------------------------------------------------
  Assembly
-------------------------------------------------------------------------------}

-- | List all cards under the specified category
listUnder :: Category -> Flashcards -> Flashcards
listUnder main (Flashcards cards) = Flashcards $
    map (first $ below main) cards

-- | Group cards by category
group ::
     [Category] -- ^ List of categories (to determine the order)
  -> [(Category, Flashcard)]
  -> Flashcards
group cats =
    go Map.empty
  where
    go ::
         Map Category [Flashcard]
      -> [(Category, Flashcard)]
      -> Flashcards
    go acc [] =
        Flashcards [
            (cat, reverse $ Map.findWithDefault [] cat acc)
          | cat <- cats
          ]
    go acc ((cat, card):cards) =
        go (Map.alter (Just . (card:) . fromMaybe []) cat acc) cards

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

numCategories :: Flashcards -> Word
numCategories (Flashcards cards) = fromIntegral . length $ cards

numCards :: Flashcards -> Word
numCards (Flashcards cards) = fromIntegral . length . concatMap snd $ cards

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

serialize :: Flashcards -> String
serialize (Flashcards cats) =
      unlines
    . map (uncurry serializeCat)
    $ cats

serializeCat :: Category -> [Flashcard] -> String
serializeCat cat cards = intercalate "\n" $
      serializeCatHeader cat
    : map serializeCard cards

serializeCard :: Flashcard -> String
serializeCard (Flashcard c) = [c]

serializeCatHeader :: Category -> String
serializeCatHeader (Category cs) = "//" ++ intercalate "/" cs
