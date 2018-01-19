module Data.Multihash.Base where

-- import qualified Data.ByteString.Base32      as B32
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Either (Either)
import qualified Data.Hex as B16

-- TODO add Base32 encoding
data BaseEncoding
  = Base2 -- ^ Raw binary encoding
  | Base16 -- ^ Hexadecimal encoding
  | Base58 -- ^ Bitcoin encoding
  | Base64
  deriving (Show, Read, Eq, Enum, Bounded)

encode :: BaseEncoding -> ByteString -> ByteString
encode Base2 = id
encode Base16 = B16.hex
-- encode Base32 = B32.encode
encode Base58 = fromStrict . B58.encodeBase58 B58.bitcoinAlphabet . toStrict
encode Base64 = B64.encode

decode :: BaseEncoding -> ByteString -> Either String ByteString
decode Base2 = return . id
decode Base16 = B16.unhex
-- decode Base32 = B32.decode
decode Base58 =
  maybe (Left "Failed to parse") (Right . fromStrict) .
  B58.decodeBase58 B58.bitcoinAlphabet . toStrict
decode Base64 = B64.decode
