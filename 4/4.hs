{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Text (splitOn, unpack, pack)
import Data.List (transpose, all)

splits :: Eq a => a -> [a] -> [[a]]
splits s xs = go xs [[]]
  where
    go [] ys = ys
    go (x : xs) ys
      | x == s = go xs $ if null $ last ys then ys else ys ++ [[]]
      | otherwise = go xs $ init ys ++ [last ys ++ [x]]

mark :: Eq a => a -> [[[(a, Bool)]]] -> [[[(a, Bool)]]]
mark e = (map . map . map) (\(e', b) -> if e' == e then (e', True) else (e', b))

-- Returns less than legnth
check :: [[[(a, Bool)]]] -> (Bool, Int)
check xs =
  let l = length . takeWhile null $ map (filter id . map (all snd)) xs
  in
    if l == length xs then (False, 0) else (True, l)

--score :: Int -> [[(Int, Bool)]] -> Int
score lastNum =
    (* lastNum)
  . sum
  . map (foldl (\acc (n, b) -> if not b then n + acc else acc) 0)

run :: [Int] -> [[[(Int, Bool)]]] -> [[[(Int, Bool)]]] -> (Int, Int, [[[(Int, Bool)]]])
run [] _ _ = error "Game ended without a winner"
run (i : is) boards tBoards =
  let nBoards = mark i boards
      nTBoards = mark i tBoards
      (b, index) = check nBoards
      (b', index') = check nTBoards
  in
    if b
    then (i, index, nBoards)
    else
      if b'
      then (i, index', nBoards)
      else run is nBoards nTBoards

main :: IO ()
main = do
  strs <- lines <$> readFile "4.input"
  let
    inputNumbers :: [Int]
    inputNumbers = map (read . unpack) . splitOn "," $ pack (head strs)

    -- Generate boards
    boards :: [[[(Int, Bool)]]]
    boards = (map . map) (map ((, False) . read) . splits ' ') $ splits "" $ tail strs

    -- Get transposed boards
    tBoards :: [[[(Int, Bool)]]]
    tBoards = map transpose boards

    -- Run the game
    (lastNum, index, nBoards) = run inputNumbers boards tBoards

  print . score lastNum $ nBoards !! index
