module GenPleco where

import Data.Function (fix)

import Pleco.Gen.Cmdline
import Pleco.Gen.Convert
import Pleco.Gen.Convert.EnumByLesson qualified as EnumByLesson
import Pleco.Gen.Convert.PerLesson qualified as PerLesson
import Pleco.Gen.Flashcards (Flashcards, Flashcard (..))
import Pleco.Gen.Flashcards qualified as Flashcards

main :: IO ()
main = do
    Cmdline{cmd} <- getCmdline
    case cmd of
      ConvertPerLesson inp out options ->
        runConversion inp out options $ PerLesson.convert
      ConvertEnumByLesson inp out options ->
        runConversion inp out options $ EnumByLesson.convert

runConversion ::
     (Monoid w, Show w)
  => FilePath  -- ^ Input path
  -> FilePath  -- ^ Output path
  -> ConversionOptions
  -> (String -> Convert () w Flashcards) -- ^ Convert input
  -> IO ()
runConversion inp out options conv = do
    (converted, stats) <- runConvert . conv <$> readFile inp
    case converted of

      Left (Error (LineNo lineNo) err) ->
        putStrLn $ "Error on line " ++ show lineNo ++ ": "++ err

      Right cards' -> do

        let cards :: Flashcards
            cards = Flashcards.listUnder (mainCategory options) cards'

            serializer :: Flashcards.Serializer
            serializer
              | forceSingleCharacterSet options
              = fix $ \self -> (Flashcards.mkSerializer self) {
                    Flashcards.serializeCard = \(Flashcard c) ->
                      [c, '[', c, ']']
                  }

              | otherwise
              = Flashcards.defaultSerializer

        writeFile out $ Flashcards.serializeFlashcards serializer cards
        putStrLn $ Flashcards.showStats $ Flashcards.computeStats cards
        putStrLn $ "Additional statistics: " ++ show stats