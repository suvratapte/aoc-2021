-- 1
countIncreases [] = 0
countIncreases [x] = 0
countIncreases (x : y : ys) = (if x < y then 1 else 0) + countIncreases (y : ys)

-- 2
threeMeasurementSlidingWindow xs
  | length xs < 3 = []
  | otherwise = head xs + xs !! 1 + xs !! 2 : threeMeasurementSlidingWindow (tail xs)

main = do
  nums <- map read . lines <$> readFile "1.input" :: IO [Int]
  print $ countIncreases nums
  print . countIncreases $ threeMeasurementSlidingWindow nums
