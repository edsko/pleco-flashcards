module Pleco.Gen.Cmdline (
    Cmdline(..)
  , Cmd(..)
  , ConversionOptions(..)
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
    ConvertPerLesson FilePath FilePath ConversionOptions
  | ConvertEnumByLesson FilePath FilePath ConversionOptions

data ConversionOptions = ConversionOptions {
      mainCategory :: Category
    , forceSingleCharacterSet :: Bool
    }

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
        "convert-perlesson"
        (parseConvert ConvertPerLesson <*> parseConversionOptions)
        "Convert from .perlesson format"
    , sub
        "convert-enumbylesson"
        (parseConvert ConvertEnumByLesson <*> parseConversionOptions)
        "Convert from .enumbylesson format"
    ]
  where
    sub :: String -> Parser a -> String -> Mod CommandFields a
    sub cmd parser desc =
        command cmd $ info (parser <**> helper) (progDesc desc)

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

parseConversionOptions :: Parser ConversionOptions
parseConversionOptions =
        ConversionOptions
    <$> parseCategory
    <*> (switch $ mconcat [
            long "force-single-character-set"
          , help "Pretend that this character is the same in both simplified and traditional"
          ])

parseCategory :: Parser Category
parseCategory = option (Category . (:[]) <$> str) $ mconcat [
      long "category"
    , help "Label for the top-level category"
    ]




