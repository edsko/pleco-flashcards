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
    -- * Statistics
  , Stats(..)
  , computeStats
  , showStats
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
  deriving (Show, Eq, Ord)

-- | Individual flash card
--
-- The Pleco format also allows flashcards to contain pinyin pronunciation and
-- definitions, but we (currently) do not support these here.
data Flashcard = Flashcard Char
  deriving (Show, Eq, Ord)

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
  Statistics
-------------------------------------------------------------------------------}

data Stats = Stats {
      numCategories    :: Word
    , numCards         :: Word
    , countPerCategory :: [(Category, Word)]
    , dups             :: [Flashcard]
    }
  deriving (Show)

computeStats :: Flashcards -> Stats
computeStats (Flashcards cards) = Stats {
      numCategories    = fromIntegral . length $ cards
    , numCards         = fromIntegral . length . concatMap snd $ cards
    , countPerCategory = map (second (fromIntegral . length)) cards
    , dups             = computeDups $ concatMap snd cards
    }
  where
    computeDups :: [Flashcard] -> [Flashcard]
    computeDups =
          Map.keys
        . Map.filter (> 1)
        . Map.unionsWith (+)
        . map (\c -> Map.singleton c (1 :: Word))

showStats :: Stats -> String
showStats stats = intercalate "\n" [
      "Number of categories: " ++ show (numCategories stats)
    , "Total number of cards: " ++ show (numCards stats)
    , "Count per category:"
    , intercalate "\n" [
          "  " ++ serializeCatHeader cat ++ ": " ++ show count
        | (cat, count) <- countPerCategory stats
        ]
    , case dups stats of
        [] -> "No duplicates"
        _  -> "Duplicates: " ++ show (dups stats)
    ]

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
