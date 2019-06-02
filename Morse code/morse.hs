module Morse where

import Data.Char

morseTab :: [(Char, String)]
morseTab =
  [('A',".-"),('B',"-..."),('C',"-.-."),('D',"-.."),('E',".")
  ,('F',"..-."),('G',"--."),('H',"...."),('I',".."),('J',".---")
  ,('K',"-.-"),('L',".-.."),('M',"--"),('N',"-."),('O',"---")
  ,('P',".--."),('Q',"--.-"),('R',".-."),('S',"..."),('T',"-")
  ,('U',"..-"),('V',"...-"),('W',".--"),('X',"-..-")
  ,('Y',"-.--"),('Z',"--..")
  ]

normalizeText :: String -> String
normalizeText xs = [toUpper x | x <- xs, isAlpha x, (x `elem` ['a'..'z']) || (x `elem` ['A'..'Z'])] 

charToCode :: [(Char,String)] -> Char -> String
charToCode [] _ = []
charToCode (x:xs) c
                | (fst x) == c = (snd x)
                | otherwise = charToCode xs c
                
encodeToWords :: String -> [String]
encodeToWords [] = []
encodeToWords (x:xs) = [(charToCode morseTab x)] ++ encodeToWords xs

encodeString :: String -> String
encodeString x = foldr (++) [] (encodeToWords x)

codeToChar :: [(a,String)] -> String -> a
codeToChar (x:xs) s
                | (snd x) == s = (fst x)
                | otherwise = codeToChar xs s
                 
decodeWords :: [String] -> String
decodeWords [] = []
decodeWords (x:xs) = [(codeToChar morseTab x)] ++ (decodeWords xs) 

withShortestCodes :: [(Char,String)] -> [Char]
withShortestCodes xs = let ys = (map (\(a, b) -> (a, length b)) xs) in map (\(a, b) -> a) (filter (\y -> (snd y) == (snd (foldr min' (head ys) (tail ys)))) ys)
    where min' a b
                | (snd a) > (snd b)  = b
                | otherwise = a
 
getPossiblePrefixes :: [(Char,String)] -> String -> [(Char,String)]
getPossiblePrefixes [] _ = []
getPossiblePrefixes (x:xs) s
                    | (snd x) == (take (length (snd x))  s) = [x] ++ (getPossiblePrefixes xs s)
                    | otherwise = getPossiblePrefixes xs s

decodeString :: String -> [String]
decodeString [] = []
decodeString s = fmap (\(a,b) -> a) (decodeString' (fmap (\(a, b) -> ([a], drop (length b) s)) (getPossiblePrefixes morseTab s)))
    where   decodeString' [] = []
            decodeString' (x:xs) = if ((snd x) /= "") 
                                     then (decodeString' (fmap (\y -> ((fst x)++[(fst y)], drop (length (snd y)) (snd x))) (getPossiblePrefixes morseTab (snd x)))) ++ (decodeString' xs)
                                     else [x] ++ (decodeString' xs)