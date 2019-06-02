module Compression where

import Data.List
import Data.Maybe

type CodeTab = [TabEntry]

type TabEntry = (Char, Code)

type Code = [Bit]

data Bit = Zero | One
  deriving (Eq,Show)

getFrequencies :: String -> [(Int, Char)]
getFrequencies xs = sortBy (\(a1, b1) (a2, b2) -> compare b1 b2) (getFrequencies' xs)
    where getFrequencies' [] = []
          getFrequencies' (x:xs) =  getFrequencies'' x (getFrequencies' xs)
          getFrequencies'' c [] = [(1,c)]
          getFrequencies'' c (x:xs) = if c == (snd x)
                                        then
                                            [((fst x) + 1, snd x)] ++ xs
                                        else
                                            [x] ++ getFrequencies'' c xs
                                            
orderByFrequency :: String -> [Char]
orderByFrequency [] = []
orderByFrequency xs = map (\(x, y) -> y) (sortBy sort' (getFrequencies xs))
    where sort'(a1, b1) (a2, b2)
                | a1 < a2 = GT
                | a1 > a2 = LT
                | otherwise = compare b1 b2

nextPrefixCode :: Code -> Code
nextPrefixCode [] = [Zero]
nextPrefixCode [Zero] = [One, Zero]
nextPrefixCode (x:xs) = [x] ++ (nextPrefixCode xs)

prefixCodes :: [Code]
prefixCodes = iterate (nextPrefixCode) [Zero]

getTab :: String -> CodeTab
getTab xs = getTab' (sortBy sort' (getFrequencies xs)) 1
    where   getTab' [] n = []
            getTab' (x:[]) n = if n > 1 then (let c = (last (take n prefixCodes)) in [(snd x, (take (n - 2) c) ++ [One])]) else  [(snd x, [Zero])]
            getTab' (x:xs) n = [(snd x, (last (take n prefixCodes)))] ++ (getTab' xs (n + 1))
            sort'(a1, b1) (a2, b2)
                | a1 < a2 = GT
                | a1 > a2 = LT
                | otherwise = compare b1 b2

lookupCode :: CodeTab -> Char -> Code
lookupCode (x:xs) c | c == (fst x) = snd x
                    | otherwise = lookupCode xs c
                    
encode :: String -> (CodeTab,Code)
encode s = let c = (getTab s) in (c, (encode' s c))
    where  encode' [] _ = []
           encode' (x:xs) c = (lookupCode c x) ++ encode' xs c 
           
lookupPrefix :: (CodeTab,Code) -> TabEntry
lookupPrefix ((x:xs),c)
                  | (snd x) == take (length (snd x)) c = x
                  | otherwise = lookupPrefix (xs, c)                

decode :: (CodeTab,Code) -> String
decode (_, []) = []
decode (t, c) = let x = (lookupPrefix (t, c)) in [fst x] ++ decode (t, (drop (length (snd x)) c))

