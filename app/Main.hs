module Main (main) where

import Options.Applicative

import Audiocate (Command (..), run, version)

data Opts = Opts
  { optVerboseFlag :: !Bool
  , optCommand :: !Command
  }

main :: IO ()
main = do
  (opts :: Opts) <- execParser optsParser
  putStrLn $ "\nAudiocate v" ++ version
  putStrLn $ "\nRunning command " ++ show (optCommand opts) ++ 
              " (verbose: " ++ show (optVerboseFlag opts) ++ ")\n"
  rc <- run (optCommand opts)
  print rc
 where
  optsParser :: ParserInfo Opts
  optsParser =
    info
      (helper <*> versionOpt <*> programOpts)
      ( fullDesc
          <> progDesc "audicate"
          <> header
            "audiocate - encode and decode audio files to authenticate their source"
      )
  versionOpt :: Parser (a -> a)
  versionOpt = infoOption version (long "version" <> help "Show version")
  programOpts :: Parser Opts
  programOpts =
    Opts
      <$> switch (long "verbose" <> help "Set to print verbose log messages")
      <*> hsubparser (encodeCommand <> helpCommand <> decodeCommand)
  encodeCommand :: Mod CommandFields Command
  encodeCommand =
    command
      "encode"
      (info encodeOpts (progDesc "Encode a WAVE audio file"))
  encodeOpts :: Parser Command
  encodeOpts =
    Encode
      <$> strArgument (metavar "SECRET" <> help "secret key to be used for encoding")
      <*> argument auto (metavar "TIMERANGE" <> help "number of seconds to for encoding to be valid for")
      <*> strArgument (metavar "INPUTFILE" <> help "input file to read for encoding")
      <*> strArgument (metavar "OUTPUTFILE" <> help "destination file to write encoded file to")
  decodeCommand :: Mod CommandFields Command
  decodeCommand =
    command
      "decode"
      (info decodeOpts (progDesc "Decode an audiocate encoded WAVE audio file"))
  decodeOpts :: Parser Command
  decodeOpts =
    Decode
      <$> strArgument (metavar "SECRET" <> help "secret key to be used for decoding")
      <*> argument auto (metavar "TIMERANGE" <> help "number of seconds used in encoding window")
      <*> strArgument (metavar "INPUTFILE" <> help "input file to read for encoding")
  helpCommand :: Mod CommandFields Command
  helpCommand =
    command
      "help"
      (info (pure Help) (progDesc "Show the full help page"))
