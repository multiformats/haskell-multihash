{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crypto.Hash (Digest)
import qualified Crypto.Hash as CH
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Options.Applicative

import System.IO.Streams (InputStream, stdin, stdout, withFileAsInput, write)
import System.IO.Streams.Crypto (hashInputStream)

import qualified Data.Multihash.Base as MB
import qualified Data.Multihash.Digest as MH

data Termination
  = Null
  | Newline
  deriving (Show, Eq)

data Config = Config
  { cfFile :: Maybe FilePath
  , cfAlgo :: MH.HashAlgorithm
  , cfBase :: MB.BaseEncoding
  , cfHash :: Maybe MH.Digest
  , cfTerm :: Termination
  } deriving (Show)

main :: IO ()
main
    -- TODO add file checking
 = do
  config <- execParser opts
  digest <- maybe (hashStdin config) (hashFile config) $ cfFile config
  write (multihash config digest) stdout
  where
    hashStdin config = hash (cfAlgo config) stdin
    hashFile config file = withFileAsInput file . hash $ cfAlgo config
    multihash (Config _file algo base _hash term) =
      Just . toStrict . line term . MB.encode base . MH.encode algo
    line Null = (<> "\0")
    line Newline = (<> "\n")

hash :: MH.HashAlgorithm -> InputStream ByteString -> IO MH.Digest
hash MH.SHA1 is = convert <$> (hashInputStream is :: IO (Digest CH.SHA1))
hash MH.SHA256 is = convert <$> (hashInputStream is :: IO (Digest CH.SHA256))
hash MH.SHA512 is = convert <$> (hashInputStream is :: IO (Digest CH.SHA512))
hash MH.SHA3 is = convert <$> (hashInputStream is :: IO (Digest CH.SHA3_256))
hash MH.BLAKE2B is =
  convert <$> (hashInputStream is :: IO (Digest CH.Blake2b_512))
hash MH.BLAKE2S is =
  convert <$> (hashInputStream is :: IO (Digest CH.Blake2s_256))

opts :: ParserInfo Config
opts =
  info
    (helper <*>
     (Config <$> fileArg <*> algoOpt <*> baseOpt <*> checkOpt <*> nullTermFlag))
    (fullDesc <> header "Generate a multihash for the given input." <>
     progDesc "Hash from FILE or stdin if not given.")

algoOpt :: Parser MH.HashAlgorithm
algoOpt =
  option auto $
  long "algorithm" <> short 'a' <> metavar "ALGO" <> showDefault <>
  value MH.SHA256 <>
  help
    ("Hash algorithm to apply to input, ignored if checking hash " <>
     show ([minBound ..] :: [MH.HashAlgorithm]))

baseOpt :: Parser MB.BaseEncoding
baseOpt =
  option auto $
  long "encoding" <> short 'e' <> metavar "ENCODING" <> showDefault <>
  value MB.Base58 <>
  help
    ("Base encoding of output digest, ignored if checking hash " <>
     show ([minBound ..] :: [MB.BaseEncoding]))

checkOpt :: Parser (Maybe MH.Digest)
checkOpt =
  optional . option auto $
  long "check" <> short 'c' <> metavar "DIGEST" <>
  help "Check for matching digest"

nullTermFlag :: Parser Termination
nullTermFlag =
  flag Newline Null $
  long "print0" <> short '0' <>
  help "End filenames with NUL, for use with xargs"

fileArg :: Parser (Maybe FilePath)
fileArg = optional . argument str $ metavar "FILE"
