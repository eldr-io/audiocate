module Main
  ( main
  ) where

import Options.Applicative
  ( CommandFields
  , Mod
  , Parser
  , ParserInfo
  , argument
  , auto
  , command
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , infoOption
  , long
  , metavar
  , progDesc
  , strArgument
  , switch
  )

import Audiocate (Command(..), run, version)

data Opts =
  Opts
    { optVerboseFlag :: !Bool
    , optCommand :: !Command
    }

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  putStrLn $ "\nAudiocate v" ++ version
  putStrLn $
    "\nRunning command " ++
    show (optCommand opts) ++
    " (verbose: " ++ show (optVerboseFlag opts) ++ ")\n"
  rc <- run (optCommand opts)
  print rc
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOpt <*> programOpts)
        (fullDesc <>
         progDesc "audiocate" <>
         header
           "audiocate - encode and decode audio files to authenticate their source")
    versionOpt :: Parser (a -> a)
    versionOpt = infoOption version (long "version" <> help "Show version")
    programOpts :: Parser Opts
    programOpts =
      Opts <$>
      switch (long "verbose" <> help "Set to print verbose log messages") <*>
      hsubparser
        (encodeCommand <>
         encodeStreamCommand <>
         helpCommand <> decodeCommand <> decodeStreamCommand)
    encodeCommand :: Mod CommandFields Command
    encodeCommand =
      command "encode" (info encodeOpts (progDesc "Encode a WAVE audio file"))
    encodeOpts :: Parser Command
    encodeOpts =
      Encode <$>
      strArgument
        (metavar "SECRET" <> help "secret key to be used for encoding") <*>
      argument
        auto
        (metavar "TIMERANGE" <>
         help "number of seconds to for encoding to be valid for") <*>
      strArgument
        (metavar "INPUTFILE" <> help "input file to read for encoding") <*>
      strArgument
        (metavar "OUTPUTFILE" <>
         help "destination file to write encoded file to")
    encodeStreamCommand :: Mod CommandFields Command
    encodeStreamCommand =
      command
        "encodestream"
        (info encodeStreamOpts (progDesc "Encode a WAVE audio stream in chunks"))
    encodeStreamOpts :: Parser Command
    encodeStreamOpts =
      EncodeStream <$>
      strArgument
        (metavar "SECRET" <> help "secret key to be used for encoding") <*>
      argument
        auto
        (metavar "TIMERANGE" <>
         help "number of seconds to for encoding to be valid for") <*>
      strArgument
        (metavar "INPUTFILE" <>
         help "input stream source file to read for encoding") <*>
      strArgument
        (metavar "OUTPUTFILE" <>
         help "destination file to stream encoded chunks to")
    decodeCommand :: Mod CommandFields Command
    decodeCommand =
      command
        "decode"
        (info
           decodeOpts
           (progDesc "Decode an audiocate encoded WAVE audio file"))
    decodeOpts :: Parser Command
    decodeOpts =
      Decode <$>
      strArgument
        (metavar "SECRET" <> help "secret key to be used for decoding") <*>
      argument
        auto
        (metavar "TIMERANGE" <> help "number of seconds used in encoding window") <*>
      strArgument
        (metavar "INPUTFILE" <> help "input file to read for encoding")
    decodeStreamCommand :: Mod CommandFields Command
    decodeStreamCommand =
      command
        "decodestream"
        (info
           decodeStreamOpts
           (progDesc "Decode an audiocate encoded WAVE audio stream in chunks"))
    decodeStreamOpts :: Parser Command
    decodeStreamOpts =
      DecodeStream <$>
      strArgument
        (metavar "SECRET" <> help "secret key to be used for decoding") <*>
      argument
        auto
        (metavar "TIMERANGE" <> help "number of seconds used in encoding window") <*>
      strArgument
        (metavar "INPUTFILE" <> help "input file to read for encoding")
    helpCommand :: Mod CommandFields Command
    helpCommand =
      command "help" (info (pure Help) (progDesc "Show the full help page"))
