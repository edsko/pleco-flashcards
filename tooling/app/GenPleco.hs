module GenPleco where

import Pleco.Gen.Cmdline
import Pleco.Gen.Convert
import Pleco.Gen.Convert.PerChapter qualified as PerChapter
import Pleco.Gen.Flashcards (Flashcards)
import Pleco.Gen.Flashcards qualified as Flashcards

main :: IO ()
main = do
    Cmdline{cmd} <- getCmdline
    case cmd of
      ConvertPerChapter inp out cat ->
        runConversion inp out $
          fmap (Flashcards.listUnder cat) . PerChapter.convert

runConversion ::
     (Monoid w, Show w)
  => FilePath  -- ^ Input path
  -> FilePath  -- ^ Output path
  -> (String -> Convert () w Flashcards) -- ^ Convert input
  -> IO ()
runConversion inp out conv = do
    (converted, stats) <- runConvert . conv <$> readFile inp
    case converted of
      Left (Error (LineNo lineNo) err) ->
        putStrLn $ "Error on line " ++ show lineNo ++ ": "++ err
      Right cards -> do
        writeFile out $ Flashcards.serialize cards
        putStrLn $ Flashcards.showStats $ Flashcards.computeStats cards
        putStrLn $ "Additional statistics: " ++ show stats