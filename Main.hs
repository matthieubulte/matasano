
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Bits              as BI
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import qualified Data.Char              as C
import           Data.Function
import           Data.Functor
import qualified Data.List              as L
import           Numeric                (showHex)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

letters :: String
letters = filter C.isPrint [C.chr 0 .. C.chr 255]

orderedFreqEnglishLetters :: [Char]
orderedFreqEnglishLetters = " etaoinhsrdlumcwgfyp,.bkv\"\\'-?xj;!qz'"

toHex :: B.ByteString -> String
toHex = concatMap (`showHex` "") . B.unpack

toB16 :: B.ByteString -> B.ByteString
toB16 = fst . B16.decode

stringToBits :: String -> [Bool]
stringToBits = concatMap charToBits

charToBits :: Char -> [Bool]
charToBits c = BI.testBit (C.ord c) <$> [0..8]

getChunk :: Int -> Int -> [a] -> [a]
getChunk l o =  take l . drop o

chunk :: Int -> String -> [String]
chunk l s = L.takeWhile (not . L.null) chunks
    where
        offsets = iterate (l +) 0
        chunks  = ($ s) <$> (getChunk l) <$> offsets

getBsChunk :: Int -> Int -> B.ByteString -> B.ByteString
getBsChunk l o = B.take l . B.drop o

bsChunk :: Int -> B.ByteString -> [B.ByteString]
bsChunk l s = L.takeWhile (not . B.null) chunks
    where
        offsets = iterate (l +) 0
        chunks  = ($ s) <$> (getBsChunk l) <$> offsets


intDiv :: Int -> Int -> Float
intDiv x y = (fromIntegral x) / (fromIntegral y)

toPair :: a -> (a, a)
toPair x = (x, x)

xor :: B.ByteString -> B.ByteString -> B.ByteString
xor a b = B.pack $ B.zipWith BI.xor a b

xor1 :: Char -> B.ByteString -> B.ByteString
xor1 c s = xor s $ (BC.pack . L.replicate (B.length s) ) c

xorN :: String -> B.ByteString -> B.ByteString
xorN k s = xor s (BC.pack key)
    where
        key = take (B.length s) (cycle k)

best1Xor :: B.ByteString -> (Char, B.ByteString, Int)
best1Xor s = last result
    where
        xors           = (`xor1` s) <$> letters
        xorToLetter    = L.zip letters xors
        unsortedResult = (\(letter, str) -> (letter, str, score $ BC.unpack str)) <$> xorToLetter
        result         = L.sortBy (compare `on` third) unsortedResult

histogram :: String -> [(Char, Int)]
histogram s = L.zip (L.head <$> ls) (L.length <$> ls)
    where
        ls = L.group $ L.sort s

rankedLetters :: String -> [Char]
rankedLetters s = L.reverse $ fst <$> L.sortBy (compare `on` snd) (histogram s)

score :: String -> Int
score s = (*) (-1) $ L.sum $ distance <$> indexedRank
    where
        localRank = rankedLetters s
        indexedRank = [0, 1..] `L.zip` localRank
        distance (i, c) = case (L.elemIndex c orderedFreqEnglishLetters) of
            Nothing -> L.length s
            Just i' -> abs (i - i')

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

-- Challenge 5
challenge5 :: Bool
challenge5 = xored == expected
    where
        original = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
        expected = toB16 "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
        key      = "ICE"
        xored    = key `xorN` original

-- Challenge 6

challenge6 :: IO [([Char], BC.ByteString)]
challenge6 = do
    contentB64          <- readFile "challenge6.txt"

    let (Right content) = B64.decode (BC.pack contentB64)
    let strContent      = BC.unpack content

    let probableKeyLengths = bestKeyLengths 5 strContent

    -- for each key length L, split the content in chunks of size L
    let chunks             = (`bsChunk` content) <$> probableKeyLengths

    -- transpose the chunks
    let transposed         = B.transpose <$> chunks

    -- for each chunk, get the best single xor character
    let keyPerBlock        = (fmap . fmap) best1Xor transposed

    print $ (fmap . fmap) (\(k, _, s) -> (k, s)) keyPerBlock

    -- join the best xor character of each block into the xor key
    let keys               = (fmap . fmap) first keyPerBlock

    let pretty             = (mapSnd $ (`xorN` content)) <$> toPair <$> keys

    return pretty

bestKeyLengths :: Int -> String -> [Int]
bestKeyLengths m s = fst <$> take m scoredLengths
    where
        lengthToScore = (mapSnd $ scoreKeyLength s) <$> toPair <$> [2..40]
        scoredLengths = L.sortBy (compare `on` snd) lengthToScore

scoreKeyLength :: String -> Int -> Float
scoreKeyLength s l = (hamming chunk1 chunk2) `intDiv` l
    where
        chunk1 = getChunk (l - 1) 0 s
        chunk2 = getChunk (2 * l) l s

hamming :: String -> String -> Int
hamming s s' = length . filter (not . id) $ L.zipWith (==) bs bs'
    where
        bs  = stringToBits s
        bs' = stringToBits s'

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

    putStr "Challenge 5: "
    print challenge5

    putStr "Challenge 6: "
    challenge6 >>= print

