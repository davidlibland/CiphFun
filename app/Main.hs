module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import RailFence
import CodeWord
import Cipher

data EncodeOrDecode = Encode | Decode deriving Show
data Cipers = RailFence | CodeWord deriving Show


-- We use optparse-applicative to define a CLI.
-- We want the user to provide the following:
data CliOptions = CliOptions
  { direction      :: EncodeOrDecode,  -- Do we encode or decode?
    cipherType     :: Cipers, -- Which cipher do we use?
    configPath     :: String, -- Where is the config file for the cipher?
    message        :: String -- What is the message (to encode/decode)
    } deriving Show

-- These parsers just choose options based on flags being set in the cli
decodeParser :: Parser EncodeOrDecode
decodeParser = flag' Decode (  long "decode" <> short 'd' <> help "Whether to decode")
encodeParser :: Parser EncodeOrDecode
encodeParser = flag' Encode (  long "encode" <> short 'e' <> help "Whether to encode")

-- Combine the two options, exactly one must be set:
encodeOrDecodeParser :: Parser EncodeOrDecode
encodeOrDecodeParser = decodeParser <|> encodeParser


-- These parsers just choose options based on flags being set in the cli
railFenceParser :: Parser Cipers
railFenceParser = flag' RailFence (  long "rail-fence" <> help "Use the rail fence cipher")
codeWordParser :: Parser Cipers
codeWordParser = flag' CodeWord (  long "code-word" <> help "Use the rail fence cipher")

-- Combine the two options, exactly one must be set:
cipherTypeParser :: Parser Cipers
cipherTypeParser = railFenceParser <|> codeWordParser

-- Finally combine all the parsers (monadically)
-- It will return a CLIOptions datum
cliParser :: Parser CliOptions
cliParser = CliOptions
      <$> -- Below the parsers combine to return a tuple, hitting it with `CliOptions <$>` returns a CliOptions.
       encodeOrDecodeParser -- Either Encode or Decode must be chosend
      <*> cipherTypeParser  -- One of the cipher must be chosen
      <*> argument str  -- An argument must be passed
          ( metavar "CONFIG"
         <> help "The YAML config file for the cipher" )
      <*> argument str  -- An argument must be passed
          ( metavar "MESSAGE"
         <> help "The message to be encoded/decoded" )

main :: IO ()
-- execParser opts will return an IO( CLIOptions )
main = runCommand =<< execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Encode or decode a message using a cipher"
     <> header "CiphFun - a cli to use playful ciphers" )

--
printResult :: Show a => Either String a -> IO ()
printResult (Left err) = print $ "Error! " ++ err
printResult (Right msg) = print msg

-- runCommand runs the cipher based on the options.
runCommand :: CliOptions -> IO ()
runCommand (CliOptions Encode RailFence _ msg) = printResult $ unwords <$> encode standardRailFenceConfig msg
runCommand (CliOptions Decode RailFence _ msg) = printResult $ decode standardRailFenceConfig $ words msg
runCommand (CliOptions Encode CodeWord _ msg) = printResult $ unwords <$> encode standardCodeWordConfig msg
runCommand (CliOptions Decode CodeWord _ msg) = printResult $ decode standardCodeWordConfig $ words msg
runCommand x = print x
