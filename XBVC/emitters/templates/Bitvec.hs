{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Bitvec
Description : Extensible bit vector encoding/decoding
Copyright   : (c) KeyMe Inc, 2018
License     : MIT
Maintainer  : opensource@key.me
Stability   : Beta
Portability : POSIX


Extensible bit vectors are an endian independent way of encoding
arbitrary width integers (signed or otherwise) into a delimited stream
of bytes.  This is especially useful for serialization as it allows
adjacent encoding of fields without requiring 'tags' (the downside
being that one must know the order of encoding!).  One additional
benefit is that integers will ONLY use the number of bytes required,
so if (for example) you had a uint32 of the value 1, only a single
byte is required to serialize (rather than 4).

Fields are delimited by an octet with the highest order bit being zero
indicating the end of a field.

-}

module Bitvec
    ( decodeST
    , decodeFloatST
    , decodeListOfST
    , decodeListST
    , decodeFloatListST
    , decodeSTUntilEmpty
    , decodeList
    , decodeFloatList
    , endDecode
    , justOnEnd
    , encodeS
    , encodeFloatS
    , encodeListS
    , encodeFloatListS
    , encodeSNew
    , encode
    , Bitvec.encodeFloat
    , decode) where

import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word

-- These functions use StateT monad so decoding can be done in
-- a do block without explicitely passing the resulting bytestring
-- from the output of one action to the input of the next.

-- | Makes an instance of StateT to be used to decode
-- bit vectors inside of do blocks.
-- StateT BS.ByteString Maybe a =
--     { runST :: BS.ByteString -> Maybe (BS.ByteString, a) }
-- Use StateT instead of State because the result is a monad
decodeST :: (Integral a) => StateT BS.ByteString Maybe a
decodeST = StateT decode

-- | An instance of StateT to be used when decoding floats
decodeFloatST :: StateT BS.ByteString Maybe Float
decodeFloatST = StateT Bitvec.decodeFloat

-- | Helper function for decoding a list of finite list of
-- values from a byte string given the decode function and the
-- length of the list.
decodeListOfST :: (BS.ByteString -> Maybe (a, BS.ByteString))
               -> Int
               -> StateT BS.ByteString Maybe [a]
decodeListOfST f n = StateT decodeListOfST'
    where
        decodeListOfST' bs = decodeListOf bs f n

-- | Function used to decode a list of a given length of integral
-- values from a bytestring using the StateT monad
decodeListST :: (Integral a) => Int -> StateT BS.ByteString Maybe [a]
decodeListST = decodeListOfST decode

-- | Function used to decode a list of a given length of float values
-- from a bytestring using the StateT monad
decodeFloatListST :: Int -> StateT BS.ByteString Maybe [Float]
decodeFloatListST = decodeListOfST Bitvec.decodeFloat

-- | Function used to decode what should be the last portion of the
-- bytestring using the StateT. If the bytestring isn't empty after
-- the value was decoded will return Nothing (decode failed)
decodeSTUntilEmpty :: StateT BS.ByteString Maybe a -> BS.ByteString -> Maybe a
decodeSTUntilEmpty stateT = evalStateT stateT'
    where stateT' = do
                        -- example of using the StateT instances
                        -- get the decoded value
                        res <- stateT
                        -- StateT takes care of passing resulting ByteString
                        -- Checks if the ByteString is null, if not returns
                        -- nothing which will short circuit the do because
                        -- of the Maybe monad
                        endDecode
                        -- only returns this value if the stateT isn't Nothing
                        -- and endDecode isn't Nothing
                        return res

-- | StateT instance of the function that checks that the ByteString
-- has been completely decoded. Type matches other decode types so it
-- can be used with the other decode functions.
endDecode :: StateT BS.ByteString Maybe ()
endDecode = StateT endS'
    where
        endS' bs
            | BS.null bs = Just ((), BS.empty)
            | otherwise = Nothing

-- | Helper function for checking if the ByteString has ended
-- Can't be used in the StateT monad
justOnEnd :: BS.ByteString -> a -> Maybe a
justOnEnd bs a
    | BS.null bs = Just a
    | otherwise = Nothing

-- Since encode should never fail we use State instead of StateT
-- because we don't need an internal Maybe monad. The State instance
-- uses type unit because there is no values that matter when encoding,
-- just the new bytestring.

-- | Function to apply value to the State instance of the encode
-- function so we can use the do block when encoding bitvectors.
-- Since this accumulates we don't need a second return type
-- so that is set to ()
encodeS :: (Integral a) => a -> State BS.ByteString ()
-- state is a helper constructor
-- wrap to return a () to match state types
encodeS a = state $ \bs -> ((), BS.append bs (encode a))

-- | Function to apply float value to the State instance of the
-- encodeFloat function so we can use the
-- do block when encoding bitvectors
encodeFloatS :: Float -> State BS.ByteString ()
encodeFloatS a = state $ \bs -> ((), BS.append bs (Bitvec.encodeFloat a))

-- | Helper function to encode a list of into bytestring given the encode
-- function and the number of items to be encoded.
encodeListOfS :: (a -> State BS.ByteString ()) -> Int -> [a] -> State BS.ByteString ()
-- Can just use basic monad map fuction because of State type
encodeListOfS f n l = mapM_ f list
    where
        list = take n l

-- | Encodes a list of integers given the number that should be encoded
encodeListS :: (Integral a) => Int -> [a] -> State BS.ByteString ()
encodeListS = encodeListOfS encodeS

-- | Encodes a list of floats given the number that should be encoded
encodeFloatListS :: Int -> [Float] -> State BS.ByteString ()
encodeFloatListS = encodeListOfS encodeFloatS

-- | Helper function to init an empty ByteString
encodeSNew :: State BS.ByteString () -> BS.ByteString
encodeSNew s = execState s BS.empty


-- | Turns a float into a list of digits
fracDigits' :: Float -> [Int]
fracDigits' 0 = []
fracDigits' f = [firstDigit] ++ fracDigits' remainder
  where
    sanitized = f - ((fromInteger . floor) f)
    shifted = sanitized * 10
    firstDigit = floor shifted
    remainder = shifted - (fromIntegral firstDigit)

-- | Given a floating point number, returns the fractional part as a
-- list of integers, e.g.: 1.0234 => [0, 2, 3, 3, 9, 9, 9...]
fracDigits :: Float -> [Int]
fracDigits 0 = [0]
fracDigits f = fracDigits' $ abs f

-- | Turns an integer value into a list of digits
intDigits' :: Integral a => a -> [Int]
intDigits' 0 = []
intDigits' n = firstDigit : intDigits' q
  where
    (q, r) = quotRem n 10
    firstDigit = fromIntegral r

-- | Given an integer, returns each digit in a list by place such that
-- if you multiplied each value by an increasing power of 10, you'd
-- get the reconstructed number, e.g 1234 => [4, 3, 2, 1] * [1, 10, 100, 1000] => 1234
intDigits :: Integral a => a -> [Int]
intDigits 0 = [0]
intDigits n = intDigits' $ abs n


-- | Returns a lazy list of multiples of 10, starting at 1
-- => [1, 10, 100]
-- pop POP *makes the raise the roof arm motion*
-- - Magnitude of Community
magnitudeList :: [Int]
magnitudeList = scanl (*) 1 $ repeat 10

-- | Returns a lazy list of divisions of 10, starting at 0.1
-- => [.1,.01, .001...]
magnitudeList' :: [Float]
magnitudeList' = scanl (/) 0.1 $ repeat 10

-- | Given a zero or (any number), returns a multiplier that will
-- properly set the sign (in our float encoding, 0 represents
-- positive, and anything else is negative)
signMul :: Integral a => a -> Float
signMul 0 = 1.0
signMul _ = (-1.0)

-- | Convert a list of ints to a list of floats
floatify :: [Int] -> [Float]
floatify = fmap fromIntegral

-- | Encode a floating point number into an EBV
encodeFloat :: Float -> BS.ByteString
encodeFloat f = BS.concat [signEnc, wholeEnc, fracEnc]
  where
    sign n | n < 0 = 1
           | otherwise = 0
    signEnc = encode $ ((sign f) :: Integer)
    wholeEnc = encode $ (((floor . abs) f) :: Integer)
    maxPrecision = 9
    decimals = take maxPrecision $ fracDigits f
    pairs = zip decimals magnitudeList
    fracEnc = encode $ reconstitute pairs

-- | Decode an EBV (stored in a bytestring) into a floating point
-- number and any remaining bytes
decodeFloat :: BS.ByteString -> Maybe (Float, BS.ByteString)
decodeFloat = runStateT $ do
  signRaw  <- decodeST :: StateT BS.ByteString Maybe Word8
  wholeRaw <- decodeST :: StateT BS.ByteString Maybe Word32
  fracRaw  <- decodeST :: StateT BS.ByteString Maybe Word32
  let sm = signMul signRaw
      wholeFlt = fromIntegral wholeRaw
      fracDig = (floatify . intDigits) $ toInteger fracRaw
      fracFlt = reconstitute $ zip fracDig magnitudeList'
      result = sm * (wholeFlt + fracFlt) :: Float
  return result

-- | Encodes an integral as an extensible bit vector represented by a bytestring
encode :: (Integral a) => a -> BS.ByteString
encode 0 = BS.singleton 0
encode n = BS.cons encodedNum (encode quotient)
  where
    n64 = fromIntegral n :: Word64
    (quotient, remainder) = quotRem n64 128
    byte = fromIntegral remainder :: Word8
    encodedNum = if n64 > 0 then (byte .|. 0x80) else byte

-- | Returns a list if integers of length n in the form [128^0, 128^1..128^n]
mulTbl :: [Int]
mulTbl = scanl (*) 1 $ repeat 128

-- | Returns a 2-tuple of EBVs representing the first fully terminated
-- EBV encountered in the input Bytestring, and a ByteString
-- containing the remaining bytes
unconsEBV :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
unconsEBV e
  | BS.null e = Nothing
  | otherwise = Just (first, remainder)
  where
    (initial, remTail) = BS.span (\x -> x .&. 0x80 == 0x80) e
    first = BS.snoc initial (BS.head remTail)
    remainder = BS.tail remTail

-- | Converts a ByteString to a list of ints
bsToIntList :: BS.ByteString -> [Int]
bsToIntList bs = map fromIntegral $ BS.unpack bs

-- | Given a ByteString representing an EBV, returns a ByteString with
-- every element masked with 127 (0x7f) to remove the most
-- significant bit
stripMSb :: BS.ByteString -> BS.ByteString
stripMSb bs = BS.map (127 .&.) bs

-- | Given a list of 2-tuples of integers, sums the product of each pair
reconstitute :: Num a => [(a, a)] -> a
reconstitute l = foldl (+) 0 (map (\(x,y) -> x * y) l)

-- | Attempts to extract and decode the first extensible bit vector in a
-- ByteString. Returns an optional pair holding an integral and any remaining
-- bytes. The process (roughly) translates to extract -> mask msbs -> zip bytes
-- with multipliers -> sum products of zipped list pairs
decode :: (Integral a) => BS.ByteString -> Maybe (a, BS.ByteString)
decode bs = case unconsEBV bs of
  Nothing -> Nothing
  Just (leading, remainder) ->
    if (BS.last leading .&. 0x80) == 0x80
    then Nothing
    else Just (result, remainder)
    where
      intLeading = (bsToIntList . stripMSb) leading
      conjoinedPairs = zip intLeading mulTbl
      result = (fromIntegral. reconstitute) conjoinedPairs


-- | Helper method for decoding lists of objects
decodeListOf :: BS.ByteString -> (BS.ByteString -> Maybe (a, BS.ByteString)) -> Int -> Maybe ([a], BS.ByteString)
decodeListOf bs _ 0 = Just ([], bs)
decodeListOf bs f count = runStateT (do
    -- example of using the StateT functions to decode
    -- decodes the first value when runStateT passes the bs into StateT f
    val <- StateT f
    -- recursively decode the rest of the list
    rest <- decodeListOfST f (count - 1)
    -- return the head and the rest concatted together
    return $ val : rest) bs

-- | Given a bytestring and a number, attempts to decode a list of
-- numbers. Returns an option of the list and any remaining data.
decodeList :: (Integral a) => BS.ByteString -> Int -> Maybe ([a], BS.ByteString)
decodeList bs count = decodeListOf bs decode count

-- | Given a bytestring and a number, attempts to decode a list of
-- floating point numbers (length == number). Returns an option of the
-- list and any remaining data
decodeFloatList :: BS.ByteString -> Int -> Maybe([Float], BS.ByteString)
decodeFloatList bs count = decodeListOf bs Bitvec.decodeFloat count
