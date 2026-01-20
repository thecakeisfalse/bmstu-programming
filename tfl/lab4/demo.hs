
-- WARNING: just playing around Haskell, implementation is written in Rust. here you can only find optimized version

import Data.List (isPrefixOf, isSuffixOf)
import Data.Sequence (Seq, (|>), (<|), empty, fromList)
import Data.Foldable (toList)

splitBy :: Char -> String -> [String]
splitBy c = foldr f [""]
  where 
    f ch s@(x:xs)
      | c == ch = []:s
      | otherwise = (ch:x):xs

-- Match (a|b\1b)* and return \1
matchLeft :: String -> Maybe String
matchLeft = inner empty
  where
    isPrefixOfSeq :: Seq Char -> String -> Bool
    isPrefixOfSeq s t = and (zipWith (==) (toList s) t)

    inner :: Seq Char -> String -> Maybe String
    inner g []
      | g == empty = Nothing
      | otherwise  = Just (toList g)
    inner _ ('a':xs) = inner (fromList "a") xs
    inner g s@('b':xs)
      | g `isPrefixOfSeq` xs,
        let after = drop (length g) xs,
        ('b':t) <- after
        = inner (('b' <| g) |> 'b') t
    inner _ _ = Nothing

-- Match b^n a b^m a b^n
matchCenter :: Int -> String -> Bool
matchCenter n s = case splitBy 'a' s of
  p@[l, c, r] |
    all (all (== 'b')) p,
    length l == n,
    length r == n,
    length c <= 2*n
    -> True
  _ -> False

optimParse :: String -> Bool
optimParse s = case splitBy 'c' s of
  [l, c, r] |
    Just ref1 <- matchLeft l,
    r == ref1,
    let n = (length ref1 - 1) `div` 2,
    matchCenter n c || c == ref1
    -> True
  _ -> False

main :: IO ()
main = do
  putStr "Hello, world!"
