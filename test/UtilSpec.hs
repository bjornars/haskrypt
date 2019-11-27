{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UtilSpec
  ( spec,
  )
where

import Data.Char
import Import
import qualified RIO.ByteString as B
import Test.Hspec
import Util (bytestring_to_w32, chunk, sha_padding, w32_to_w8)

breplicate :: Int -> Char -> ByteString
breplicate n = B.replicate n . fromIntegral . ord

spec :: Spec
spec = do
  describe "chunk" $ do
    it "handles empty input" $ chunk 4 "" `shouldBe` ([], Nothing)
    it "handles chunk-sized input" $
      chunk 4 "xxxx" `shouldBe` (["xxxx"], Nothing)
    it "handles many-chunk-sized input" $
      chunk 4 (breplicate 60 'x') `shouldBe` (replicate 15 "xxxx", Nothing)
    it "handles off-sized input" $
      chunk 4 "xxxxy" `shouldBe` (["xxxx"], Just "y")
    it "handles off-sized many-chunk input" $
      chunk 4 "zzzzxxxxyyy" `shouldBe` (["zzzz", "xxxx"], Just "yyy")
  describe "bytestring_to_w32" $ do
    it "extracts Word32 from start of bytestring" $
      bytestring_to_w32 0 (B.pack [1, 2, 3, 4]) `shouldBe` 0x01020304
    it "extracts Word32 from middle of bytestring" $
      bytestring_to_w32 2 (B.pack [1, 2, 3, 4, 5, 6, 7, 8]) `shouldBe` 0x03040506
  describe "w32 to w8" $ do
    it "can convert 0" $ do
      w32_to_w8 0 `shouldBe` [0, 0, 0, 0]
    it "can convert numbers" $ do
      w32_to_w8 0x12345678 `shouldBe` [0x12, 0x34, 0x56, 0x78]
  describe "sha_padding" $ do
    it "pads empty input" $ do
      let res = sha_padding 0
      B.length res `shouldBe` 64
      res `shouldBe`( B.pack $ 0x80 : (replicate 63 0))
    it "pads small input" $ do
      let res = sha_padding 4
      B.length res `shouldBe` 60
      res `shouldBe` (B.pack $ 0x80 : (replicate 51 0) <> [0, 0, 0, 0, 0, 0, 0, 4 * 8])
