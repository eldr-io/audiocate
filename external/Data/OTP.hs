{-# LANGUAGE CPP #-}

{- | SPDX-License-Identifier: MIT

Implements the /HMAC-Based One-Time Password Algorithm/ (HOTP) as
defined in [RFC 4226](https://tools.ietf.org/html/rfc4226)
and the /Time-Based One-Time Password Algorithm/ (TOTP) as defined
in [RFC 6238](https://tools.ietf.org/html/rfc6238).

Many operations in this module take or return a 'Word32' OTP value
(whose most significant bit is always 0) which is truncated modulo
@10^digits@ according to the 'Word8' /digits/
parameter. Consequently, passing a value above 10 won't produce
more than 10 digits and will effectively return the raw
non-truncated 31-bit OTP value.

THIS SOURCE CODE IS ORIGINALLY FROM https://github.com/haskell-hvr/OTP
AND HAS BEEN MODIFIED TO BE COMPILED WITH A NEWER GHC VERSION

-}
module Data.OTP (
  -- * HOTP
  hotp,
  hotpCheck,

  -- * TOTP
  totp,
  totpCheck,

  -- * Auxiliary
  totpCounter,
  counterRange,
  totpCounterRange,
  HashAlgorithm (..),
  Secret,
)
where

import Crypto.Hash.SHA1 qualified as SHA1
import Crypto.Hash.SHA256 qualified as SHA256
import Crypto.Hash.SHA512 qualified as SHA512
import Data.Bits
import Data.ByteString qualified as BS
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word

-- | Shared secret encoded as raw octets
type Secret = BS.ByteString

-- | Hash algorithm used for HOTP\/TOTP computations
data HashAlgorithm
  = SHA1
  | SHA256
  | SHA512
  deriving (Eq, Show)

hmac :: HashAlgorithm -> Secret -> BS.ByteString -> BS.ByteString
hmac alg key msg = case alg of
  SHA1 -> SHA1.hmac key msg
  SHA256 -> SHA256.hmac key msg
  SHA512 -> SHA512.hmac key msg

{- | Compute /HMAC-Based One-Time Password/ using secret key and counter value.

>>> hotp SHA1 "1234" 100 6
WAS 317569
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA1 "1234" 100 6
NOW In an equation for `it_a2YnU': it_a2YnU = hotp SHA1 "1234" 100 6
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS 134131
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA512 "1234" 100 6
NOW In an equation for `it_a2YpW': it_a2YpW = hotp SHA512 "1234" 100 6
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA1 "1234" 100 6
WAS 55134131
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA512 "1234" 100 8
NOW In an equation for `it_a2YrY': it_a2YrY = hotp SHA512 "1234" 100 8
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS 134131
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA512 "1234" 100 6
NOW In an equation for `it_a2Y3u': it_a2Y3u = hotp SHA512 "1234" 100 6
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA1 "1234" 100 6
WAS 55134131
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA512 "1234" 100 8
NOW In an equation for `it_a2Y5w': it_a2Y5w = hotp SHA512 "1234" 100 8

>>> hotp SHA512 "1234" 100 6
WAS 134131
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA512 "1234" 100 6
NOW In an equation for `it_a2XH2': it_a2XH2 = hotp SHA512 "1234" 100 6

>>> hotp SHA512 "1234" 100 8
WAS 55134131
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA512 "1234" 100 8
NOW In an equation for `it_a2XJ4': it_a2XJ4 = hotp SHA512 "1234" 100 8
-}
hotp ::
  -- | Hashing algorithm
  HashAlgorithm ->
  -- | Shared secret
  Secret ->
  -- | Counter value
  Word64 ->
  -- | Number of base10 digits in HOTP value
  Word8 ->
  -- | HOTP value
  Word32
hotp alg key cnt digits
  | digits >= 10 = snum
  | otherwise = snum `rem` 10 ^ digits
 where
  -- C
  msg = bsFromW64 cnt

  -- Snum  = StToNum(Sbits)
  -- Sbits = DT(HS)
  -- HS    = HMAC(K,C)
  snum = trunc $ hmac alg key msg

  -- DT(HS)
  trunc :: BS.ByteString -> Word32
  trunc b = case bsToW32 rb of
    Left e -> error e
    Right res -> res .&. (0x80000000 - 1) -- reset highest bit
   where
    offset = BS.last b .&. 15 -- take low 4 bits of last byte
    rb = BS.take 4 $ BS.drop (fromIntegral offset) b -- resulting 4 byte value

  -- StToNum(Sbits)
  bsToW32 :: BS.ByteString -> Either String Word32
  bsToW32 bs = case BS.unpack bs of
    [b0, b1, b2, b3] -> Right $! ((((fI b0 `shiftL` 8) .|. fI b1) `shiftL` 8) .|. fI b2) `shiftL` 8 .|. fI b3
    _ -> Left "bsToW32: the impossible happened"
   where
    fI = fromIntegral

  bsFromW64 :: Word64 -> BS.ByteString
  bsFromW64 w = BS.pack [b j | j <- [7, 6 .. 0]]
   where
    b j = fromIntegral (w `shiftR` (j * 8))

{- | Check presented password against a valid range.

>>> hotp SHA1 "1234" 10 6
WAS 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA1 "1234" 10 6
NOW In an equation for `it_a2X42': it_a2X42 = hotp SHA1 "1234" 10 6
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In an equation for `it_a2X64':
NOW     it_a2X64 = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
NOW In the expression: hotp SHA1 "1234" 10 6
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW In an equation for `it_a2X8o':
NOW     it_a2X8o = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2XaI':
NOW     it_a2XaI = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2Xd2':
NOW     it_a2Xd2 = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2Xfm':
NOW     it_a2Xfm = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2Wv6 = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2XhG':
NOW     it_a2XhG = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2Xk0':
NOW     it_a2Xk0 = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Xmk':
NOW     it_a2Xmk = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Wxq':
NOW     it_a2Wxq = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2WzK':
NOW     it_a2WzK = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2WC4':
NOW     it_a2WC4 = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2WEo':
NOW     it_a2WEo = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2VU8 = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2WGI':
NOW     it_a2WGI = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2WJ2':
NOW     it_a2WJ2 = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2WLm':
NOW     it_a2WLm = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2VWs':
NOW     it_a2VWs = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2VYM':
NOW     it_a2VYM = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2W16':
NOW     it_a2W16 = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2W3q':
NOW     it_a2W3q = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2Vja = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2W5K':
NOW     it_a2W5K = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2W84':
NOW     it_a2W84 = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Wao':
NOW     it_a2Wao = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Vlu':
NOW     it_a2Vlu = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2VnO':
NOW     it_a2VnO = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2Vq8':
NOW     it_a2Vq8 = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2Vss':
NOW     it_a2Vss = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2UIc = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2VuM':
NOW     it_a2VuM = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2Vx6':
NOW     it_a2Vx6 = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Vzq':
NOW     it_a2Vzq = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2UKw':
NOW     it_a2UKw = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2UMQ':
NOW     it_a2UMQ = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2UPa':
NOW     it_a2UPa = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2URu':
NOW     it_a2URu = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2U7e = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2UTO':
NOW     it_a2UTO = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2UW8':
NOW     it_a2UW8 = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2UYs':
NOW     it_a2UYs = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2U9y':
NOW     it_a2U9y = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2UbS':
NOW     it_a2UbS = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2Uec':
NOW     it_a2Uec = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2Ugw':
NOW     it_a2Ugw = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2Twg = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2UiQ':
NOW     it_a2UiQ = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2Ula':
NOW     it_a2Ula = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Unu':
NOW     it_a2Unu = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2TyA':
NOW     it_a2TyA = hotpCheck SHA1 "1234" (0, 0) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2TAU':
NOW     it_a2TAU = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2TDe':
NOW     it_a2TDe = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2TFy':
NOW     it_a2TFy = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2SVi = hotpCheck SHA1 "1234" (0, 0) 10 6 50897
NOW In the second argument of `hotp', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2THS':
NOW     it_a2THS = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2TKc':
NOW     it_a2TKc = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2TMw':
NOW     it_a2TMw = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2SXC':
NOW     it_a2SXC = hotpCheck SHA1 "1234" (0, 0) 9 6 50897

>>> hotpCheck SHA1 "1234" (0,0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2SZW':
NOW     it_a2SZW = hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2T2g':
NOW     it_a2T2g = hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 0) 10 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2T4A':
NOW     it_a2T4A = hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW     it_a2Ski = hotpCheck SHA1 "1234" (0, 0) 10 6 50897

WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2T6U':
NOW     it_a2T6U = hotpCheck SHA1 "1234" (2, 2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2T9e':
NOW     it_a2T9e = hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2Tby':
NOW     it_a2Tby = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2SmC':
NOW     it_a2SmC = hotpCheck SHA1 "1234" (0, 0) 9 6 50897

>>> hotpCheck SHA1 "1234" (0,1) 9 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (0, 1) 9 6 50897
NOW In an equation for `it_a2SoW':
NOW     it_a2SoW = hotpCheck SHA1 "1234" (0, 1) 9 6 50897

>>> hotpCheck SHA1 "1234" (1,0) 11 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (1, 0) 11 6 50897
NOW In an equation for `it_a2Srg':
NOW     it_a2Srg = hotpCheck SHA1 "1234" (1, 0) 11 6 50897

>>> hotpCheck SHA1 "1234" (2,2) 8 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 8 6 50897
NOW In an equation for `it_a2StA':
NOW     it_a2StA = hotpCheck SHA1 "1234" (2, 2) 8 6 50897

>>> hotpCheck SHA1 "1234" (2,2) 7 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 7 6 50897
NOW In an equation for `it_a2SvU':
NOW     it_a2SvU = hotpCheck SHA1 "1234" (2, 2) 7 6 50897

>>> hotpCheck SHA1 "1234" (2,2) 12 6 50897
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 12 6 50897
NOW In an equation for `it_a2Syg':
NOW     it_a2Syg = hotpCheck SHA1 "1234" (2, 2) 12 6 50897

>>> hotpCheck SHA1 "1234" (2,2) 13 6 50897
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `hotpCheck', namely `"1234"'
NOW In the expression: hotpCheck SHA1 "1234" (2, 2) 13 6 50897
NOW In an equation for `it_a2SAA':
NOW     it_a2SAA = hotpCheck SHA1 "1234" (2, 2) 13 6 50897
-}
hotpCheck ::
  -- | Hash algorithm to use
  HashAlgorithm ->
  -- | Shared secret
  Secret ->
  -- | Valid counter range, before and after ideal
  (Word8, Word8) ->
  -- | Ideal (expected) counter value
  Word64 ->
  -- | Number of base10 digits in a password
  Word8 ->
  -- | Password (i.e. HOTP value) entered by user
  Word32 ->
  -- | True if password is valid
  Bool
hotpCheck alg secr rng cnt len pass =
  let counters = counterRange rng cnt
      passwds = map (\c -> hotp alg secr c len) counters
   in elem pass passwds

{- | Compute a /Time-Based One-Time Password/ using secret key and time.

>>> totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
WAS 388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
NOW In an equation for `it_a2RTo':
NOW     it_a2RTo = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In an equation for `it_a2RVw':
NOW     it_a2RVw = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2RXE':
NOW     it_a2RXE = totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2Ruw':
NOW     it_a2Ruw = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
WAS 39110359
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
NOW In an equation for `it_a2RZM':
NOW     it_a2RZM = totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
NOW Expected: Secret
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In an equation for `it_a2RwE':
NOW     it_a2RwE = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2RyM':
NOW     it_a2RyM = totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2R5E':
NOW     it_a2R5E = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
WAS 39110359
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
NOW In an equation for `it_a2RAU':
NOW     it_a2RAU = totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
NOW Expected: Secret
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In an equation for `it_a2R7M':
NOW     it_a2R7M = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2R9U':
NOW     it_a2R9U = totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2QGM':
NOW     it_a2QGM = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
WAS 39110359
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
NOW In an equation for `it_a2Rc2':
NOW     it_a2Rc2 = totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
>>> totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
NOW In an equation for `it_a2QIU':
NOW     it_a2QIU = totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8

>>> totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
WAS 43388892
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
NOW In an equation for `it_a2QL2':
NOW     it_a2QL2 = totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8

>>> totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
WAS 39110359
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
NOW In an equation for `it_a2QNa':
NOW     it_a2QNa = totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
-}
totp ::
  -- | Hash algorithm to use
  HashAlgorithm ->
  -- | Shared secret
  Secret ->
  -- | Time of TOTP
  UTCTime ->
  -- | Time range in seconds
  Word64 ->
  -- | Number of base10 digits in TOTP value
  Word8 ->
  -- | TOTP value
  Word32
totp alg secr time period = hotp alg secr (totpCounter time period)

{- | Check presented password against time periods.

>>> totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
WAS 778374
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
NOW   totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
NOW In an equation for `it_a2Qcc':
NOW     it_a2Qcc = totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In an equation for `it_a2Qek':
NOW     it_a2Qek
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2QgK':
NOW     it_a2QgK
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2PHC':
NOW     it_a2PHC = totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2Qja':
NOW     it_a2Qja
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW Expected: Secret
WAS True
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2QlA':
NOW     it_a2QlA
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW Expected: Secret
NOW   Actual: String
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2Qo0':
NOW     it_a2Qo0
NOW       = totpCheck
NOW           SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In an equation for `it_a2PJK':
NOW     it_a2PJK
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2PMa':
NOW     it_a2PMa
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2Pd2':
NOW     it_a2Pd2 = totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2POA':
NOW     it_a2POA
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW Expected: Secret
WAS True
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2PR0':
NOW     it_a2PR0
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW Expected: Secret
NOW   Actual: String
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2PTq':
NOW     it_a2PTq
NOW       = totpCheck
NOW           SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In an equation for `it_a2Pfa':
NOW     it_a2Pfa
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2PhA':
NOW     it_a2PhA
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2OIs':
NOW     it_a2OIs = totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2Pk0':
NOW     it_a2Pk0
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW Expected: Secret
WAS True
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2Pmq':
NOW     it_a2Pmq
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW Expected: Secret
NOW   Actual: String
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2PoQ':
NOW     it_a2PoQ
NOW       = totpCheck
NOW           SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In an equation for `it_a2OKA':
NOW     it_a2OKA
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2ON0':
NOW     it_a2ON0
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2OdS':
NOW     it_a2OdS = totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2OPq':
NOW     it_a2OPq
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW Expected: Secret
WAS True
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2ORQ':
NOW     it_a2ORQ
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW Expected: Secret
NOW   Actual: String
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2OUg':
NOW     it_a2OUg
NOW       = totpCheck
NOW           SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In an equation for `it_a2Og0':
NOW     it_a2Og0
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In the second argument of `totp', namely `"1234"'
NOW In the expression:
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2Oiq':
NOW     it_a2Oiq
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2NJc':
NOW     it_a2NJc = totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2OkQ':
NOW     it_a2OkQ
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
>>> totpCheck SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
WAS True
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2Ong':
NOW     it_a2Ong
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW Expected: Secret
NOW   Actual: String
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2OpG':
NOW     it_a2OpG
NOW       = totpCheck
NOW           SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
NOW In an equation for `it_a2NLm':
NOW     it_a2NLm
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374

>>> totpCheck SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2NNM':
NOW     it_a2NNM
NOW       = totpCheck
NOW           SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374

>>> totpCheck SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
NOW In an equation for `it_a2NQe':
NOW     it_a2NQe
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374

>>> totpCheck SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
WAS False
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2NSE':
NOW     it_a2NSE
NOW       = totpCheck
NOW           SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374

>>> totpCheck SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
WAS True
NOW Couldn't match type `[Char]' with `ByteString'
NOW Expected: Secret
NOW   Actual: String
NOW In the second argument of `totpCheck', namely `"1234"'
NOW In the expression:
NOW   totpCheck
NOW     SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
NOW In an equation for `it_a2NV6':
NOW     it_a2NV6
NOW       = totpCheck
NOW           SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
-}
totpCheck ::
  -- | Hash algorithm to use
  HashAlgorithm ->
  -- | Shared secret
  Secret ->
  -- | Valid counter range, before and after ideal
  (Word8, Word8) ->
  -- | Time of TOTP
  UTCTime ->
  -- | Time range in seconds
  Word64 ->
  -- | Number of base10 digits in a password
  Word8 ->
  -- | Password given by user
  Word32 ->
  -- | True if password is valid
  Bool
totpCheck alg secr rng time period len pass =
  let counters = totpCounterRange rng time period
      passwds = map (\c -> hotp alg secr c len) counters
   in elem pass passwds

{- | Calculate HOTP counter using time. Starting time (T0
according to RFC6238) is 0 (begining of UNIX epoch)

>>> totpCounter (read "2010-10-10 00:00:00 UTC") 30
42888960

>>> totpCounter (read "2010-10-10 00:00:30 UTC") 30
42888961

>>> totpCounter (read "2010-10-10 00:01:00 UTC") 30
42888962
-}
totpCounter ::
  -- | Time of totp
  UTCTime ->
  -- | Time range in seconds
  Word64 ->
  -- | Resulting counter
  Word64
totpCounter time period =
  let timePOSIX = floor $ utcTimeToPOSIXSeconds time
   in timePOSIX `div` period

{- | Make a sequence of acceptable counters, protected from
arithmetic overflow.

>>> counterRange (0, 0) 9000
[9000]

>>> counterRange (1, 0) 9000
[8999,9000]

>>> length $ counterRange (5000, 0) 9000
WAS 501
NOW 137
NOW 137
NOW 137
WAS 1000
NOW 273
WAS 1000
NOW 273
[18446744073709551613,18446744073709551614,18446744073709551615]
NOW 273
[18446744073709551613,18446744073709551614,18446744073709551615]
[0,1,2]
[18446744073709551613,18446744073709551614,18446744073709551615]
[0,1,2]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[0,1,2]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]
>>> counterRange (5, 5) 9000
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]

RFC recommends avoiding excessively large values for counter ranges.
-}
counterRange ::
  -- | Number of counters before and after ideal
  (Word8, Word8) ->
  -- | Ideal counter value
  Word64 ->
  [Word64]
counterRange (tolow, tohigh) ideal = [l .. h]
 where
  l' = ideal - fromIntegral tolow
  l
    | l' <= ideal = l'
    | otherwise = 0

  h' = ideal + fromIntegral tohigh
  h
    | ideal <= h' = h'
    | otherwise = maxBound

{- | Make a sequence of acceptable periods.

>>> totpCounterRange (0, 0) (read "2010-10-10 00:01:00 UTC") 30
[42888962]

>>> totpCounterRange (2, 0) (read "2010-10-10 00:01:00 UTC") 30
[42888960,42888961,42888962]

>>> totpCounterRange (0, 2) (read "2010-10-10 00:01:00 UTC") 30
[42888962,42888963,42888964]

>>> totpCounterRange (2, 2) (read "2010-10-10 00:01:00 UTC") 30
[42888960,42888961,42888962,42888963,42888964]
-}
totpCounterRange ::
  (Word8, Word8) ->
  UTCTime ->
  Word64 ->
  [Word64]
totpCounterRange rng time period =
  counterRange rng $ totpCounter time period
