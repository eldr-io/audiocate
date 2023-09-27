{-# LANGUAGE CPP #-}
-- | SPDX-License-Identifier: MIT
--
-- Implements the /HMAC-Based One-Time Password Algorithm/ (HOTP) as
-- defined in [RFC 4226](https://tools.ietf.org/html/rfc4226)
-- and the /Time-Based One-Time Password Algorithm/ (TOTP) as defined
-- in [RFC 6238](https://tools.ietf.org/html/rfc6238).
--
-- Many operations in this module take or return a 'Word32' OTP value
-- (whose most significant bit is always 0) which is truncated modulo
-- @10^digits@ according to the 'Word8' /digits/
-- parameter. Consequently, passing a value above 10 won't produce
-- more than 10 digits and will effectively return the raw
-- non-truncated 31-bit OTP value.
--
-- THIS SOURCE CODE IS ORIGINALLY FROM https://github.com/haskell-hvr/OTP
-- AND HAS BEEN MODIFIED TO BE COMPILED WITH A NEWER GHC VERSION
--
-- @since 0.1.0.0
module Data.OTP
       ( -- * HOTP
         hotp
       , hotpCheck
         -- * TOTP
       , totp
       , totpCheck
         -- * Auxiliary
       , totpCounter
       , counterRange
       , totpCounterRange

       , HashAlgorithm(..)
       , Secret
       ) where

import           Data.Bits
import qualified Data.ByteString       as BS
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word

#if defined(MIN_VERSION_SHA)
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Digest.Pure.SHA as SHA
#else
import qualified Crypto.Hash.SHA1     as SHA1
import qualified Crypto.Hash.SHA256   as SHA256
import qualified Crypto.Hash.SHA512   as SHA512
#endif

import qualified Data.ByteString      as BS


-- | Shared secret encoded as raw octets
type Secret = BS.ByteString

-- | Hash algorithm used for HOTP\/TOTP computations
data HashAlgorithm = SHA1
                   | SHA256
                   | SHA512
                   deriving (Eq,Show)

hmac :: HashAlgorithm -> Secret -> BS.ByteString -> BS.ByteString
hmac alg key msg = case alg of
#if defined(MIN_VERSION_SHA)
    SHA1   -> BS.L.toStrict (SHA.bytestringDigest (SHA.hmacSha1   (BS.L.fromStrict key) (BS.L.fromStrict msg)))
    SHA256 -> BS.L.toStrict (SHA.bytestringDigest (SHA.hmacSha256 (BS.L.fromStrict key) (BS.L.fromStrict msg)))
    SHA512 -> BS.L.toStrict (SHA.bytestringDigest (SHA.hmacSha512 (BS.L.fromStrict key) (BS.L.fromStrict msg)))
#else
    SHA1   -> SHA1.hmac   key msg
    SHA256 -> SHA256.hmac key msg
    SHA512 -> SHA512.hmac key msg
#endif

{- | Compute /HMAC-Based One-Time Password/ using secret key and counter value.

>>> hotp SHA1 "1234" 100 6
317569

>>> hotp SHA512 "1234" 100 6
134131

>>> hotp SHA512 "1234" 100 8
55134131

-}

hotp
  :: HashAlgorithm           -- ^ Hashing algorithm
  -> Secret                  -- ^ Shared secret
  -> Word64                  -- ^ Counter value
  -> Word8                   -- ^ Number of base10 digits in HOTP value
  -> Word32                  -- ^ HOTP value
hotp alg key cnt digits
  | digits >= 10 = snum
  | otherwise    = snum `rem` (10 ^ digits)
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
                Left e    -> error e
                Right res -> res .&. (0x80000000 - 1) -- reset highest bit
      where
        offset = BS.last b .&. 15 -- take low 4 bits of last byte
        rb = BS.take 4 $ BS.drop (fromIntegral offset) b -- resulting 4 byte value

    -- StToNum(Sbits)
    bsToW32 :: BS.ByteString -> Either String Word32
    bsToW32 bs = case BS.unpack bs of
                   [ b0, b1, b2, b3 ] -> Right $! (((((fI b0 `shiftL` 8) .|. fI b1) `shiftL` 8) .|. fI b2) `shiftL` 8) .|. fI b3
                   _                  -> Left "bsToW32: the impossible happened"
      where
        fI = fromIntegral

    bsFromW64 :: Word64 -> BS.ByteString
    bsFromW64 w = BS.pack [ b j | j <- [ 7, 6 .. 0 ] ]
      where
        b j = fromIntegral (w `shiftR` (j*8))

{- | Check presented password against a valid range.

>>> hotp SHA1 "1234" 10 6
50897

>>> hotpCheck SHA1 "1234" (0,0) 10 6 50897
True

>>> hotpCheck SHA1 "1234" (0,0) 9 6 50897
False

>>> hotpCheck SHA1 "1234" (0,1) 9 6 50897
True

>>> hotpCheck SHA1 "1234" (1,0) 11 6 50897
True

>>> hotpCheck SHA1 "1234" (2,2) 8 6 50897
True

>>> hotpCheck SHA1 "1234" (2,2) 7 6 50897
False

>>> hotpCheck SHA1 "1234" (2,2) 12 6 50897
True

>>> hotpCheck SHA1 "1234" (2,2) 13 6 50897
False

-}

hotpCheck
  :: HashAlgorithm      -- ^ Hash algorithm to use
  -> Secret             -- ^ Shared secret
  -> (Word8, Word8)     -- ^ Valid counter range, before and after ideal
  -> Word64             -- ^ Ideal (expected) counter value
  -> Word8              -- ^ Number of base10 digits in a password
  -> Word32             -- ^ Password (i.e. HOTP value) entered by user
  -> Bool               -- ^ True if password is valid
hotpCheck alg secr rng cnt len pass =
    let counters = counterRange rng cnt
        passwds = map (\c -> hotp alg secr c len) counters
    in any (pass ==) passwds

{- | Compute a /Time-Based One-Time Password/ using secret key and time.

>>> totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 6
388892

>>> totp SHA1 "1234" (read "2010-10-10 00:01:00 UTC") 30 8
43388892

>>> totp SHA1 "1234" (read "2010-10-10 00:01:15 UTC") 30 8
43388892

>>> totp SHA1 "1234" (read "2010-10-10 00:01:31 UTC") 30 8
39110359

-}

totp
  :: HashAlgorithm -- ^ Hash algorithm to use
  -> Secret    -- ^ Shared secret
  -> UTCTime   -- ^ Time of TOTP
  -> Word64    -- ^ Time range in seconds
  -> Word8     -- ^ Number of base10 digits in TOTP value
  -> Word32    -- ^ TOTP value
totp alg secr time period len =
    hotp alg secr (totpCounter time period) len

{- | Check presented password against time periods.

>>> totp SHA1 "1234" (read "2010-10-10 00:00:00 UTC") 30 6
778374

>>> totpCheck SHA1 "1234" (0, 0) (read "2010-10-10 00:00:00 UTC") 30 6 778374
True

>>> totpCheck SHA1 "1234" (0, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
False

>>> totpCheck SHA1 "1234" (1, 0) (read "2010-10-10 00:00:30 UTC") 30 6 778374
True

>>> totpCheck SHA1 "1234" (1, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
False

>>> totpCheck SHA1 "1234" (2, 0) (read "2010-10-10 00:01:00 UTC") 30 6 778374
True
-}

totpCheck
  :: HashAlgorithm      -- ^ Hash algorithm to use
  -> Secret             -- ^ Shared secret
  -> (Word8, Word8)     -- ^ Valid counter range, before and after ideal
  -> UTCTime            -- ^ Time of TOTP
  -> Word64             -- ^ Time range in seconds
  -> Word8              -- ^ Number of base10 digits in a password
  -> Word32             -- ^ Password given by user
  -> Bool               -- ^ True if password is valid
totpCheck alg secr rng time period len pass =
    let counters = totpCounterRange rng time period
        passwds = map (\c -> hotp alg secr c len) counters
    in any (pass ==) passwds


{- | Calculate HOTP counter using time. Starting time (T0
according to RFC6238) is 0 (begining of UNIX epoch)

>>> totpCounter (read "2010-10-10 00:00:00 UTC") 30
42888960

>>> totpCounter (read "2010-10-10 00:00:30 UTC") 30
42888961

>>> totpCounter (read "2010-10-10 00:01:00 UTC") 30
42888962

-}

totpCounter
  :: UTCTime     -- ^ Time of totp
  -> Word64      -- ^ Time range in seconds
  -> Word64      -- ^ Resulting counter
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
501

>>> length $ counterRange (5000, 5000) 9000
1000

>>> counterRange (2, 2) maxBound
[18446744073709551613,18446744073709551614,18446744073709551615]

>>> counterRange (2, 2) minBound
[0,1,2]

>>> counterRange (2, 2) (maxBound `div` 2)
[9223372036854775805,9223372036854775806,9223372036854775807,9223372036854775808,9223372036854775809]

>>> counterRange (5, 5) 9000
[8995,8996,8997,8998,8999,9000,9001,9002,9003,9004,9005]

RFC recommends avoiding excessively large values for counter ranges.
-}

counterRange
  :: (Word8, Word8)   -- ^ Number of counters before and after ideal
  -> Word64           -- ^ Ideal counter value
  -> [Word64]
counterRange (tolow, tohigh) ideal = [l..h]
  where
    l' = ideal - fromIntegral tolow
    l | l' <= ideal = l'
      | otherwise  = 0

    h' = ideal + fromIntegral tohigh
    h | ideal <= h' = h'
      | otherwise   = maxBound

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

totpCounterRange :: (Word8, Word8)
                 -> UTCTime
                 -> Word64
                 -> [Word64]
totpCounterRange rng time period =
    counterRange rng $ totpCounter time period

