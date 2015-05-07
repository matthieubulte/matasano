{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Bits              as BI
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import           Data.Function
import           Data.Functor
import qualified Data.List              as L

third :: (a, b, c) -> c
third (_, _, x) = x

second :: (a, b, c) -> b
second (_, x, _) = x

letters :: String
letters = ['A'..'z'] ++ ['0'..'9']

-- http://home.ccil.org/~cowan/trigrams
trigrams :: [String]
trigrams = [ " th", "the", "he ", "to "
           , " s ", "ed ", " of", " an"
           , " of", "nd ", " in", "and"
           ]

toB16 :: B.ByteString -> B.ByteString
toB16 = fst . B16.decode

xor :: B.ByteString -> B.ByteString -> B.ByteString
xor a b = B.pack $ B.zipWith BI.xor a b

xor1 :: Char -> B.ByteString -> B.ByteString
xor1 c s = xor s $ (BC.pack . L.replicate (B.length s) ) c

trigramsOf :: String -> [String]
trigramsOf []        = []
trigramsOf [x]       = [[x, ' ', ' ']]
trigramsOf [x, y]    = [[x, y, ' ']]
trigramsOf (x:(y:(z:r))) = [x, y, z] : trigramsOf (y:z:r)

score :: String -> Int
score s = length $ L.filter id $ (`elem` trigrams) <$> trigramsOf s

best1Xor :: B.ByteString -> (Char, B.ByteString, Int)
best1Xor s = last result
    where
        xors           = (`xor1` s) <$> letters
        xorToLetter    = L.zip letters xors
        unsortedResult = (\(letter, str) -> (letter, str, score $ BC.unpack str)) <$> xorToLetter
        result         = L.sortBy (compare `on` third) unsortedResult

-- Challenge 1
challenge1 :: Bool
challenge1 = result == expected
    where
        decrypted = toB16 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        expected  = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        result    = B64.encode decrypted

-- Challenge 2
challenge2 :: Bool
challenge2 = result == expected
    where
        decrypted = toB16 "1c0111001f010100061a024b53535009181c"
        xorKey    = toB16 "686974207468652062756c6c277320657965"
        expected  = toB16 "746865206b696420646f6e277420706c6179"
        result    = decrypted `xor` xorKey

-- Challenge 3
challenge3 :: (Char, BC.ByteString, Int)
challenge3 = best1Xor $ toB16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- Challenge 4
challenge4 :: IO (Char, BC.ByteString, Int)
challenge4 = do
    content      <- readFile "challenge4.txt"
    let hashes   = (toB16 . BC.pack) <$> lines content
    let bestXors = best1Xor <$> hashes
    let sorted   = L.sortBy (compare `on` third) bestXors
    return $ last sorted

main :: IO ()
main = do
    putStr "Challenge 1: "
    print challenge1

    putStr "Challenge 2: "
    print challenge2

    putStr "Challenge 3: "
    print challenge3

    putStr "Challenge 4: "
    challenge4 >>= print

