{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ShaSpec
  ( spec,
  )
where

import Data.Char
import Import
import RIO.List (repeat)
import qualified RIO.ByteString as B
import Sha (Digest (..), sha256)
import Test.Hspec

digest_from_string :: [Char] -> Digest
digest_from_string = Digest . B.pack . fmap fromIntegral . go
  where
    go (x1 : x2 : xs) = (16 * char_to_nibble x1 + (char_to_nibble x2)) : go xs
    go [] = mempty
    go _ = undefined
    char_to_nibble c | c >= '0' && c <= '9' = ord c - ord '0'
    char_to_nibble c | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
    char_to_nibble c | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
    char_to_nibble _ = undefined

spec :: Spec
spec = do
  describe "sha256" $ do
    it "handles empty input" $ do
      sha256 "" `shouldBe` digest_from_string "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    it "handles small input" $ do
      sha256 "hest" `shouldBe` digest_from_string "b831b3e336ecb131ebc5014393c084bf5d1580854212f64df9ed9226feebc4ae"
    it "handles medium-small input" $ do
      sha256 (mconcat . take 64 $ repeat "h") `shouldBe` digest_from_string "b831b3e336ecb131ebc5014393c084bf5d1580854212f64df9ed9226feebc4ae"
    it "handles medium input" $ do
      sha256 (mconcat . take 1000 $ repeat "hesterfint") `shouldBe` digest_from_string "b831b3e336ecb131ebc5014393c084bf5d1580854212f64df9ed9226feebc4ae"
