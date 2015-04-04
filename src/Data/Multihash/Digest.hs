module Data.Multihash.Digest where


import           Data.Attoparsec.ByteString (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BS
import           Data.ByteString.Builder    (Builder, byteString,
                                             toLazyByteString)
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as LB
import           Data.Monoid                ((<>))
import           Data.Word                  (Word8)


data MultihashDigest =
    MultihashDigest
    { function :: !HashFunction
    , lenght   :: !Length
    , digest   :: !Digest
    } deriving (Show, Eq)


type Length = Int
type Digest = BS.ByteString


data HashFunction
    = SHA1
    | SHA2256
    | SHA2512
    | SHA3
    | BLAKE2B
    | BLAKE2S
    deriving (Show, Eq)


instance Enum HashFunction where
    toEnum   = toHashFunction
    fromEnum = fromHashFunction


toHashFunction :: Int -> HashFunction
toHashFunction 0x11 = SHA1
toHashFunction 0x12 = SHA2256
toHashFunction 0x13 = SHA2512
toHashFunction 0x14 = SHA3
toHashFunction 0x40 = BLAKE2B
toHashFunction 0x41 = BLAKE2S
toHashFunction _ = error "Unknown hash funciton code"


fromHashFunction :: HashFunction -> Int
fromHashFunction SHA1    = 0x11
fromHashFunction SHA2256 = 0x12
fromHashFunction SHA2512 = 0x13
fromHashFunction SHA3    = 0x14
fromHashFunction BLAKE2B = 0x40
fromHashFunction BLAKE2S = 0x41


encode :: HashFunction -> Length -> Digest -> LB.ByteString
encode h l d = toLazyByteString $ encoder h l d


encoder :: HashFunction -> Length -> Digest -> Builder
encoder h l d
    =  (BB.word8 . fromIntegral $ fromEnum h)
    <> (BB.word8 $ fromIntegral l)
    <> byteString d


decode :: BS.ByteString -> Either String MultihashDigest
decode = parseOnly decoder


decoder :: Parser MultihashDigest
decoder = do
    h <- (toEnum . fromIntegral <$> A.anyWord8)
    l <- (fromIntegral <$> A.anyWord8)
    d <- A.take l
    return $ MultihashDigest h l d
