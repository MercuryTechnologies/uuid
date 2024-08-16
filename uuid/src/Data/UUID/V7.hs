module Data.UUID.V7 (nextRandom) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Time.Clock.System as Time
import qualified Data.UUID as UUID
import qualified Data.Word as Word
import qualified System.Entropy as Entropy

nextRandom :: IO UUID.UUID
nextRandom = do
  t <- Time.getSystemTime
  -- Note that we only need 74 bits (12 + 62) of randomness. That's a little
  -- more than 9 bytes (72 bits), so we have to request 10 bytes (80 bits) of
  -- entropy. The extra 6 bits are discarded.
  b <- Entropy.getEntropy 10
  let f = Bits.shift . word8ToWord64 . ByteString.index b
  let r = f 0 0 + f 1 8
  let s = f 2 0 + f 3 8 + f 4 16 + f 5 24 + f 6 32 + f 7 40 + f 8 48 + f 9 56
  pure $ buildUUIDv7 t r s

buildUUIDv7 ::
  Time.SystemTime ->
  -- | Only uses the low 12 bits.
  Word.Word64 ->
  -- | Only uses the low 62 bits.
  Word.Word64 ->
  UUID.UUID
buildUUIDv7 t r s =
  let unix_ts_ms =
        Bits.shift
          ( (int64ToWord64 (Time.systemSeconds t) * 1000)
              + word32ToWord64 (div (Time.systemNanoseconds t) 1000000)
          )
          16
      ver = Bits.shift 0x7 12 :: Word.Word64
      rand_a = r Bits..&. 0x0fff
      var = Bits.shift 0x2 62 :: Word.Word64
      rand_b = s Bits..&. 0x3fffffffffffffff
   in UUID.fromWords64
        (unix_ts_ms + ver + rand_a)
        (var + rand_b)

int64ToWord64 :: Int.Int64 -> Word.Word64
int64ToWord64 = fromIntegral

word8ToWord64 :: Word.Word8 -> Word.Word64
word8ToWord64 = fromIntegral

word32ToWord64 :: Word.Word32 -> Word.Word64
word32ToWord64 = fromIntegral
