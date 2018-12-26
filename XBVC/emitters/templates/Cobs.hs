module Cobs where

import Test.QuickCheck

import qualified Data.Tuple as T
import qualified Data.ByteString as BS
import qualified Data.Word as W

import qualified Data.List as L


-- uses guards because nested if then elses are pretty ugly

-- Encode a stream of bytes into a COBS formated stream of bytes
encode :: BS.ByteString -> BS.ByteString
encode bs = encode' BS.empty bs
    where
        -- Recursive function that takes a ByteString of bytes without 0s in them,
        -- the remaining ByteString to enocde and returns an encoded ByteString
        encode' :: BS.ByteString -> BS.ByteString -> BS.ByteString
        encode' nonZeros restOfBS
            -- If the number of bytes seen (and cached) so far is 254
            -- and we are at the end of the original ByteString
            -- it encodes this as 255 followed by all 254 non-zero bytes
            -- and the last zero of the string
            | BS.length nonZeros == 254 && BS.null restOfBS = BS.cons 255 (BS.snoc nonZeros 0)
            -- Else if the number of non-zero bytes is 254 and there is still
            -- more of the ByteString to enocde return 255 followed by the
            -- 254 non-zero bytes followed by the result of encoding the rest
            | BS.length nonZeros == 254 = BS.append (BS.cons 255 nonZeros)
                                                    (encode restOfBS)
            -- Else look at the front of the remainding ByteString to decide
            -- what to return
            | otherwise = let numToNextZero = fromIntegral (BS.length nonZeros + 1) in
                case BS.uncons restOfBS of
                    -- If the remaining ByteString is empty return the number of
                    -- non-zero bytes followed by the non-zero bytes ending with
                    -- the end of the ByteString 0
                    Nothing -> BS.cons numToNextZero (BS.snoc nonZeros 0)
                    -- If the first byte of the remaining ByteString is a zero
                    -- return the number of non-zero bytes followed by the
                    -- non-zero bytes followed by the encoding of the
                    -- remaining ByteString
                    (Just (0, tl)) -> BS.append (BS.cons numToNextZero nonZeros)
                                                (encode tl)
                    -- Last if the next byte isn't a zero add it to the
                    -- non-zero ByteString and recurse on the encode'
                    -- function with the remaining ByteString after
                    -- this byte
                    (Just (h, tl)) -> encode' (BS.snoc nonZeros h) tl


-- Decode a stream of COBS encoded bytes into the original byte stream
decode :: BS.ByteString -> Either String BS.ByteString
-- Check the begining of the byte string
decode bs =
    case BS.uncons bs of
        -- An empty ByteString is not properly encoded
        Nothing -> Left (errorMsg "Found empty ByteString" bs)
        -- If the front byte is a zero we have reached the end
        -- of the encoded byte string. If there are more bytes
        -- after the zero then the encoding is not correct
        (Just (0, tl)) -> if BS.null tl
            then Right BS.empty
            else Left (errorMsg "Found non-empty ByteString after 0" tl)
        -- If the next value isn't zero it is the number of
        -- bytes until the next zero
        (Just (h, tl)) ->
            -- front is the ByteString with no zeros from the original
            -- ByteString while back is the remaining ByteString to
            -- decode still (splitAt needs a Int not a Word8)
            let (front, back) = BS.splitAt (fromIntegral (h - 1)) tl
                -- if 'next zero' number is 255 or the next byte is a 0
                -- (checks for empty before using head for safety)
                -- the decoded bytes are the non-zero bytes with no ending 0
                -- else they are the non-zero bytes followed by the removed 0
                decodedChunk = if h == 255 || BS.null back || BS.head back == 0
                                    then if BS.null back
                                            then
                                                let msg = if BS.any (0 ==) front
                                                            then "Found 0 too soon"
                                                            else "No ending 0 in byte string" in
                                                Left (errorMsg msg bs)
                                            else Right front
                                    else Right (BS.snoc front 0) in
                    -- return the decoded bytestring followed by the remaining
                    -- byte string after it has been decoded
                    BS.append <$> decodedChunk <*> (decode back)
    where
        errorMsg reason' bs' = ("ByteString not encoded using COBS: " ++ reason' ++
                                " in byte string " ++ show (BS.unpack bs'))


encodeTests :: [([W.Word8], [W.Word8])]
encodeTests = [ ( [0]
                , [1, 1, 0]
                ) -- 1
              , ( [0, 0]
                , [1, 1, 1, 0]
                ) -- 2
              , ( [0x11, 0x22, 0x0, 0x33]
                , [3, 0x11, 0x22, 0x2, 0x33, 0]
                ) -- 3
              , ( [0x11, 0x22, 0x33, 0x44]
                , [5, 0x11, 0x22, 0x33, 0x44, 0]
                ) -- 4
              , ( [0x11, 0, 0, 0]
                , [2, 0x11, 1, 1, 1, 0]
                ) -- 5
              , ( [1..254]
                , 0xff : [1..0xfe] ++ [0]
                ) -- 6
              , ( [0..254]
                , [1, 0xff] ++ [1..0xfe] ++ [0]
                ) -- 7
              , ( [1..255]
                , 0xff : [1..0xfe] ++ [2, 0xff, 0]
                ) -- 8
              , ( [2..255] ++ [0]
                , 0xff : [2..0xff] ++ [1, 1, 0]
                ) -- 9
              , ( [3..255] ++ [0, 1]
                , 0xfe : [3..0xff] ++ [2, 1, 0]
                ) -- 10
              , ( [1..254] ++ [0]
                , 0xff : [1..254] ++ [1, 1, 0]
                )
              ]


decodeTests = T.swap <$> encodeTests

-- Takes a list of source byte strings
-- and the correct resulting byte strings and returns
-- a list of the number of the test, whether it passed or not
-- and the correct result and the functions result
runEncodeTests :: [([W.Word8], [W.Word8])] -> [(Int, (Bool, [W.Word8], [W.Word8]))]
runEncodeTests tests = L.zip [1..] ((\(x, y, z) -> (x, BS.unpack y, BS.unpack z)) <$> results)
    where
        packedTests = (\(x, y) -> (BS.pack x, BS.pack y)) <$> tests
        results = runTest <$> packedTests
        runTest (original, encoded) = let myEncoded = encode original in
                                        (myEncoded == encoded, encoded, myEncoded)


runDecodeTests :: [([W.Word8], [W.Word8])] -> [(Int, (Bool, [W.Word8], Either String [W.Word8]))]
runDecodeTests tests = L.zip [1..] ((\(x, y, z) -> (x, BS.unpack y, BS.unpack <$> z)) <$> results)
    where
        packedTests = (\(x, y) -> (BS.pack x, BS.pack y)) <$> tests
        results = runTest <$> packedTests
        runTest (original, decoded) = let myDecoded = decode original
                                          passed = case myDecoded of
                                                    Left _ -> False
                                                    Right result -> result == decoded
                                            in (passed, decoded, myDecoded)


-- Takes the output of runTests and returns a formated string
-- showing the results with PASSED or FAILED and if the test
-- failed the correct output and the output of the test
showResults :: Show a => [(Int, (Bool, [W.Word8], a))] -> String
showResults results = L.intercalate "\n\n" (showResult <$> results)
    where
        showResult (testNum, (passed, ideal, my)) =
            if passed then (show testNum) ++ ") PASSED"
                      else (show testNum) ++ ") FAILED\n\tGOAL: " ++
                           (show ideal) ++ "\n\tACTUAL: " ++ (show my)


-- Returns whether the original list of Word8s was returned to it's
-- original form after being encoded and decoded
checkEncodeDecode :: [W.Word8] -> Bool
checkEncodeDecode bs =
    case decode (encode (BS.pack bs)) of
        Left _ -> False
        Right result -> BS.pack bs == result

main = do
    putStrLn "Testing COBS encoding and decoding"
    putStrLn "\n\nTesting encoding"
    putStrLn $ showResults $ runEncodeTests encodeTests
    putStrLn "\n\nTesting decoding"
    putStrLn $ showResults $ runDecodeTests decodeTests
    putStrLn "\n\nTesting original == decode (encode original)"
    quickCheck checkEncodeDecode
