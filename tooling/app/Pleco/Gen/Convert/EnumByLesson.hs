module Pleco.Gen.Convert.EnumByLesson (convert) where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Text.Read (readMaybe)

import Pleco.Gen.Convert
import Pleco.Gen.Flashcards (Flashcards(..), Flashcard(..), Category (..))
import Pleco.Gen.Flashcards qualified as Flashcards

{-------------------------------------------------------------------------------
  Top-level pipeline
-------------------------------------------------------------------------------}

convert :: String -> Convert () NoStats Flashcards
convert =
      isolateState initState process
    . mapM (uncurry convertLine)
    . skipEmptyLines
    . map (second stripHashComments)
    . numberLines
    . lines

{-------------------------------------------------------------------------------
  Convert each line
-------------------------------------------------------------------------------}

convertLine :: LineNo -> String -> Convert State NoStats (Lesson, [Flashcard])
convertLine lineNo line
  | (l, _:chars) <- break isSpace line
  , Just (lesson :: Lesson) <- readMaybe l
  = do st <- get
       unless (nextLesson st == lesson) $
         throwError $ Error lineNo $ "Expected lesson " ++ show (nextLesson st)
       put $ st{nextLesson = succ lesson}
       return (lesson, map Flashcard chars)

  | otherwise
  = throwError $ Error lineNo "Unexpected input"

{-------------------------------------------------------------------------------
  Process final results
-------------------------------------------------------------------------------}

process :: State -> [(Lesson, [Flashcard])] -> Flashcards
process State{nextLesson} cards =
    Flashcards.group
      (map toCategory [1 .. pred nextLesson])
      (map (first toCategory) $ concatMap (\(l, cs) -> map (l,) cs) cards)
  where
    toCategory :: Lesson -> Flashcards.Category
    toCategory l = Category ["Lesson " ++ show l]

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

type Lesson = Word

data State = State {
      nextLesson :: Lesson
    }

initState :: State
initState = State {
      nextLesson = 1
    }