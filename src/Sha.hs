{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Sha
  ( Digest (..),
    sha256init,
    sha256add,
    sha256finish,
    sha256,
  )
where

import Constants (sha_initHash, sha_roundConstants)
import Data.Bits ((.&.), complement, rotateR, shiftR, xor)
import Data.Char
import Numeric (showHex, showIntAtBase)
import RIO
import qualified RIO.ByteString as B
import RIO.List.Partial ((!!))
import Util (bytestring_to_w32, chunk, sha_padding, w32_to_w8)

newtype Digest
  = Digest ByteString
  deriving (Eq)

hex :: (Integral a, Show a) => a -> String
hex x =
  let res = showHex x ""
   in if length res == 1
        then "0" <> res
        else res

bin :: (Integral a, Show a) => a -> String
bin x =
  let repr = chr . (+ ord ('0'))
      res = replicate 32 '0' <> showIntAtBase 2 repr x ""
   in drop (length res - 32) res

instance Show Digest where
  show (Digest b) = concatMap hex $ B.unpack b

data ShaWork
  = ShaWork
      { numBytes :: Word64,
        pendingWork :: ByteString,
        hashState :: [Word32] -- 8 long
      }
  deriving (Show, Eq)

-- get a fresh sha-state upon which we can add work
sha256init :: ShaWork
sha256init =
  ShaWork {numBytes = 0, pendingWork = B.empty, hashState = sha_initHash}

-- add an arbitrary amount of input to the work
sha256add :: ShaWork -> B.ByteString -> ShaWork
sha256add work input =
  let len = fromIntegral $ B.length input
      nextWork =
        work
          { numBytes = numBytes work + len,
            pendingWork = pendingWork work <> input
          }
   in doWork nextWork

-- process as many 512 bit chunks as are available, and store the
-- remainder for next time (or for finishing)
doWork :: ShaWork -> ShaWork
doWork work =
  let pending = pendingWork work
      state = hashState work
      (todo, remainder) = chunk 64 pending
   in work
        { pendingWork = fromMaybe B.empty remainder,
          hashState = foldl' doChunk state todo
        }

-- process a 512 bit chunk of input, modifying the hash-state in the process
doChunk ::
  [Word32] -> -- hash state, size=8
  B.ByteString -> -- 64 bytes, 512 bits
  [Word32] -- new hash state
doChunk current bs =
  let stuff = foldl' (&) current (go <$> [0 .. 63])
   in zipWith (+) current stuff
  where
    schedule = createMessageSchedule (flip bytestring_to_w32 bs <$> [0, 4 .. 63])
    go i [a, b, c, d, e, f, g, h] =
      let s1 = (e `rotateR` 6) `xor` (e `rotateR` 11) `xor` (e `rotateR` 25)
          ch = (e .&. f) `xor` ((complement e) .&. g)
          temp1 = h + s1 + ch + (sha_roundConstants !! i) + (schedule !! i)
          s0 = (a `rotateR` 2) `xor` (a `rotateR` 13) `xor` (a `rotateR` 22)
          maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
          temp2 = s0 + maj
       in [temp1 + temp2, a, b, c, d + temp1, e, f, g]
    go _ _ = undefined

-- extend 128 bits of message into 512 bits
createMessageSchedule :: [Word32] -> [Word32]
createMessageSchedule = reverse  . go 48 . reverse
  where
    go :: Int -> [Word32] -> [Word32]
    go size ws | size == 0 = ws
    go size ws'
      | otherwise =
        let ws = next : ws'
            w15 = ws !! 15
            w7 = ws !! 7
            w16 = ws !! 16
            w2 = ws !! 2
            s0 = (w15 `rotateR` 7) `xor` (w15 `rotateR` 18) `xor` (w15 `shiftR` 3)
            s1 = (w2 `rotateR` 17) `xor` (w2 `rotateR` 19) `xor` (w2 `shiftR` 10)
            next = w16 + s0 + w7 + s1
         in go (size - 1) ws

-- pad the remaining input into a 512 bit block and process,
-- and return resulting hash-state as a digest
sha256finish :: ShaWork -> Digest
sha256finish  = do
  state <- hashState
  pending <- pendingWork
  padding <- sha_padding . numBytes
  let chunk' = pending <> padding
  return $ Digest . B.pack $ concatMap w32_to_w8 $ doChunk state chunk'

sha256 :: ByteString -> Digest
sha256 = sha256finish . sha256add sha256init
