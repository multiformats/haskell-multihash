module Data.Multihash.Digest where


import           Data.Attoparsec.ByteString (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BS
import           Data.ByteString.Builder    (Builder, byteString,
                                             toLazyByteString)
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Data.Monoid                ((<>))
import           Data.Word                  (Word8)


data MultihashDigest =
    MultihashDigest
    { algorithm :: !HashAlgorithm
    , lenght    :: !Length
    , digest    :: !Digest
    } deriving (Show, Eq)


type Length = Int
type Digest = BS.ByteString


data HashAlgorithm
    = SHA1
    | SHA256
    | SHA512
    | SHA3
    | BLAKE2B
    | BLAKE2S
    deriving (Show, Eq)


instance Enum HashAlgorithm where
    toEnum   = toHashAlgorithm
    fromEnum = fromHashAlgorithm


toHashAlgorithm :: Int -> HashAlgorithm
toHashAlgorithm 0x11 = SHA1
toHashAlgorithm 0x12 = SHA256
toHashAlgorithm 0x13 = SHA512
toHashAlgorithm 0x14 = SHA3
toHashAlgorithm 0x40 = BLAKE2B
toHashAlgorithm 0x41 = BLAKE2S
toHashAlgorithm _ = error "Unknown hash funciton code"


fromHashAlgorithm :: HashAlgorithm -> Int
fromHashAlgorithm SHA1    = 0x11
fromHashAlgorithm SHA256  = 0x12
fromHashAlgorithm SHA512  = 0x13
fromHashAlgorithm SHA3    = 0x14
fromHashAlgorithm BLAKE2B = 0x40
fromHashAlgorithm BLAKE2S = 0x41


encode :: HashAlgorithm -> Digest -> BL.ByteString
encode h d = toLazyByteString $ encoder h d


encoder :: HashAlgorithm -> Digest -> Builder
encoder h d
    =  (BB.word8 . fromIntegral $ fromEnum h)
    <> (BB.word8 . fromIntegral $ BS.length d)
    <> byteString d


decode :: BS.ByteString -> Either String MultihashDigest
decode = parseOnly decoder


decoder :: Parser MultihashDigest
decoder = do
    h <- (toEnum . fromIntegral <$> A.anyWord8)
    l <- (fromIntegral <$> A.anyWord8)
    d <- A.take l
    return $ MultihashDigest h l d
