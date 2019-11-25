{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ShaSpec
  ( spec
  ) where

import Data.Char
import qualified RIO.ByteString as B

import Import
import Sha (Digest(..), sha256)
import Test.Hspec

breplicate :: Int -> Char -> ByteString
breplicate n = B.replicate n . fromIntegral . ord

spec :: Spec
spec = do
  describe "sha256" $ do
    it "handles empty input" $ do
      sha256 "" `shouldBe`
        Digest
          (B.pack
             [ 0xe3
             , 0xb0
             , 0xc4
             , 0x42
             , 0x98
             , 0xfc
             , 0x1c
             , 0x14
             , 0x9a
             , 0xfb
             , 0xf4
             , 0xc8
             , 0x99
             , 0x6f
             , 0xb9
             , 0x24
             , 0x27
             , 0xae
             , 0x41
             , 0xe4
             , 0x64
             , 0x9b
             , 0x93
             , 0x4c
             , 0xa4
             , 0x95
             , 0x99
             , 0x1b
             , 0x78
             , 0x52
             , 0xb8
             , 0x55
             ])
