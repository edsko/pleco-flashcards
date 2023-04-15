module Pleco.Gen.Cmdline (
    Cmdline(..)
  , Cmd(..)
  , getCmdline
  ) where

import Options.Applicative

import Pleco.Gen.Flashcards (Category(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmd :: Cmd
    }

data Cmd =
    ConvertPerChapter FilePath FilePath Category

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = execParser $ info (parseCmdline <**> helper) fullDesc

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
        Cmdline
    <$> parseCmd

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      sub
        "convert-per-chapter"
        (parseConvert ConvertPerChapter <*> parseCategory)
        "Convert from per-chapter format"
    ]
  where
    sub :: String -> Parser a -> String -> Mod CommandFields a
    sub cmd parser desc =
        command cmd $ info (parser <**> helper) (progDesc desc)

parseCategory :: Parser Category
parseCategory = option (Category . (:[]) <$> str) $ mconcat [
      long "category"
    , help "Label for the top-level category"
    ]

parseConvert :: (FilePath -> FilePath -> a) -> Parser a
parseConvert cmd =
        cmd
    <$> option str (mconcat [
            long "in"
          , metavar "PATH"
          , help "Input file"
          ])
    <*> option str (mconcat [
            long "out"
          , metavar "PATH"
          , help "Output file"
          ])




