-- 1

import Data.Char (digitToInt)
import Data.List (foldl', transpose)

bStrToDecimal :: String -> Int
bStrToDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

occurence :: Eq a => a -> [a] -> Int
occurence e = length . filter (== e)

onesComplement :: Char -> Char
onesComplement '0' = '1'
onesComplement '1' = '0'
onesComplement _ = error "Input not expected!"

gammaRateBinary :: [String] -> String
gammaRateBinary =
    map (\x -> if occurence '0' x > occurence '1' x then '0' else '1')
  . transpose

powerConsumption :: [String] -> Int
powerConsumption xs =
  let grb = gammaRateBinary xs
  in bStrToDecimal grb * bStrToDecimal (map onesComplement grb)

-- 2

-- Something like `index . transpose` is more elegant but this is more
-- performant.
elemsAtIndex :: Int -> [[a]] -> [a]
elemsAtIndex index = map (!! index)

oxygenBitCriteria :: String -> Char
oxygenBitCriteria xs =
  if occurence '0' xs > occurence '1' xs then '0' else '1'

co2BitCriteria :: String -> Char
co2BitCriteria xs =
  if occurence '0' xs <= occurence '1' xs then '0' else '1'

ratingBinary ::  (String -> Char) -> [String] -> String
ratingBinary fn xs = go 0 xs
  where
    go index xs
     | length xs == 1 = head xs
     | otherwise =
       let hd = elemsAtIndex index xs
           e = fn hd
       in
         go (index + 1) $ filter ((== e) . (!! index)) xs

lifeSupportRating xs =
  let ogrb = ratingBinary oxygenBitCriteria xs
      csrb = ratingBinary co2BitCriteria xs
  in
    bStrToDecimal ogrb * bStrToDecimal csrb

main :: IO ()
main = do
  strs <- lines <$> readFile "3.input"
  print $ powerConsumption strs
  print $ lifeSupportRating strs
