parseVal :: String -> (String, Int)
parseVal str =
  let [s, n] = words str
  in (s, read n)

-- 1
getDepthPos :: [String] -> (Int, Int)
getDepthPos = foldl go (0, 0)
  where
    go acc@(pos, depth) e =
      let (s, n) = parseVal e
      in
      case s of
        "forward" -> (pos + n, depth)
        "down" -> (pos, depth + n)
        "up" -> (pos, depth - n)
        _ -> acc

-- 2
getAimDepthPos :: [String] -> (Int, Int, Int)
getAimDepthPos = foldl go (0, 0, 0)
  where
    go acc@(pos, depth, aim) e =
      let (s, n) = parseVal e
      in
      case s of
        "forward" -> (pos + n, depth + aim * n, aim)
        "down" -> (pos, depth, aim + n)
        "up" -> (pos, depth, aim - n)
        _ -> acc

main :: IO ()
main = do
  strs <- lines <$> readFile "2.input"
  let (pos, depth) = getDepthPos strs
  print (pos * depth)
  let (pos, depth, _) = getAimDepthPos strs
  print (pos * depth)
