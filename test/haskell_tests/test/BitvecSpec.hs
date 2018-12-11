{-# LANGUAGE ScopedTypeVariables #-}
module BitvecSpec (spec) where

import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.ByteString as BS
import Bitvec


eq :: (Ord a, Num a) => a -> a -> a -> Bool
eq tol a b = tol > abs (a - b)

spec :: Spec
spec = do
  describe "Test Bitvec Encoding/Decoding" $ do
    prop "round-trip works" $
      \x -> Just (x :: Int, BS.empty) == (decode . encode) x

    prop "float round-trip works" $
      \x -> case (Bitvec.decodeFloat . Bitvec.encodeFloat) x of
              Just (result, _) -> eq 1e-6 result x
              Nothing -> False

    prop "Test Int list Encode/Decode works" $
      \(x :: [Int]) ->
        let
          result :: Maybe ([Int], BS.ByteString)
          encodedInts = BS.concat $ Bitvec.encode <$> x
          result = Bitvec.decodeList encodedInts (length x)
        in
        case result of
              Just (y, _) -> y == x
              _ -> False

    prop "Test Word list Encode/Decode works" $
      \(x :: [Word8]) ->
        let
          result :: Maybe ([Word8], BS.ByteString)
          encodedInts = BS.concat $ Bitvec.encode <$> x
          result = Bitvec.decodeList encodedInts (length x)
        in
        case result of
              Just (y, _) -> y == x
              _ -> False
