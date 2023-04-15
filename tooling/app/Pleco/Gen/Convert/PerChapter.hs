module Pleco.Gen.Convert.PerChapter (convert) where

import Control.Monad.Except
import Data.Bifunctor
import Text.Read

import Pleco.Gen.Convert
import Pleco.Gen.Flashcards (Flashcards(..), Category(..), Flashcard(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Pleco.Gen.Flashcards qualified as Flashcards

convert :: String -> Convert () NoStats Flashcards
convert =
      isolateState initState process
    . mapM (uncurry convertLine)
    . map (second words)
    . skipEmptyLines
    . map (second stripHashComments)
    . numberLines
    . lines

convertLine :: LineNo -> [String] -> Convert State NoStats (Lesson, Flashcard)
convertLine _ [[ch], l] | Just lesson <- readMaybe l = do
   modify $ \st@State{lessons} -> st{lessons = Set.insert lesson lessons}
   return (lesson, Flashcard ch)
convertLine lineNo _ =
   throwError $ Error lineNo "Unexpected input"

process :: State -> [(Lesson, Flashcard)] -> Flashcards
process State{lessons} cards =
    Flashcards.group
      (map toCategory $ Set.toList lessons)
      (map (first toCategory) cards)
  where
    toCategory :: Lesson -> Category
    toCategory l = Category ["Lesson " ++ show l]

type Lesson = Word

data State = State {
      lessons :: Set Lesson
    }

initState :: State
initState = State {
      lessons = Set.empty
    }
