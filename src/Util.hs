{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}

module Util
  ( bytestring_to_w32
  , chunk
  , sha_padding
  , w32_to_w8
  ) where

import Control.Arrow
import Data.Bits (Bits, (.&.), (.|.), rotateL, shiftR)
import RIO
import qualified RIO.ByteString as B

chunk :: Int -> ByteString -> ([ByteString], Maybe ByteString)
chunk size bs
  | B.null bs = ([], Nothing)
  | B.length bs < size = ([], Just bs)
  | otherwise =
    let (c, cs) = B.splitAt size bs
     in (c :) `first` chunk size cs

bytestring_to_w32 :: Int -> ByteString -> Word32
bytestring_to_w32 idx bs =
  let bytes = fmap fromIntegral . B.unpack . B.take 4 . B.drop idx $ bs
   in case reverse bytes of
        [b0, b1, b2, b3] ->
          (b0 .|. (b1 `rotateL` 8) .|. (b2 `rotateL` 16) .|. (b3 `rotateL` 24))
        _ -> undefined

byte :: (Integral a, Bits a) => a -> Int -> Word8
byte b n = fromIntegral $ (b `shiftR` n) .&. 255

w32_to_w8 :: Word32 -> [Word8]
w32_to_w8 b = reverse $ fmap (byte b) [0,8 .. 31]

sha_padding :: Word64 -> ByteString
sha_padding numBytes =
  let zeroes = (64 :: Int) - fromIntegral (numBytes + 8 + 1 `mod` 64)
      size = fmap (byte numBytes) [0 .. 7]
      in B.pack $ 0x80 : (replicate zeroes 0) ++ size
