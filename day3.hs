import Text.Parsec

data Claim = Claim { i :: !Int
                   , x :: !Int
                   , y :: !Int
                   , w :: !Int
                   , h :: !Int
                   } deriving (Eq, Show)

main :: IO ()
main = do

  strInput <- readFile "./day3_input"

  case parse parseClaims "day3" strInput of
    Left parseError  -> error (show parseError)
    Right claims -> do
        print (part1 claims)
        print (i <$> part2 [] claims)

    where
    part1 :: [Claim] -> Int
    part1 claims = countOverlapped (establishBounds claims)

      where
      countOverlapped (maxX, maxY) =
        length [ () | a <- [0..maxX+1],
                      b <- [0..maxY+1],
                      sum (map (inClaim a b) claims) >= 2]

    inClaim :: Int -> Int -> Claim -> Int
    inClaim tx ty c | tx >= x c && tx < x c + w c && ty >= y c && ty < y c + h c = 1
                    | otherwise = 0

    establishBounds claims = do
      let maxX = maximum $ map (\c -> x c + w c) claims
          maxY = maximum $ map (\c -> y c + h c) claims
      (maxX, maxY)

    part2 :: [Claim] -> [Claim] -> Maybe Claim
    part2 _ [] = Nothing
    part2 acc (c:cs) = do
        if any (\c2 -> intersect c c2) cs || any (\c2 -> intersect c c2) acc
            then part2 (c:acc) cs
            else (Just c)

      where
      intersect :: Claim -> Claim -> Bool
      intersect c1 c2 = not $ or [ x c1 + w c1 < x c2
                                 , x c2 + w c2 < x c1
                                 , y c1 + h c1 < y c2
                                 , y c2 + h c2 < y c1
                                 ]

parseClaims :: Parsec String t [Claim]
parseClaims = many parseClaim
  where
  parseClaim = do
    Claim <$> (char '#' *> num <* spaces <* char '@' <* spaces)
          <*> num <* char ','
          <*> num <* char ':' <* spaces
          <*> num <* char 'x'
          <*> num <* many endOfLine
    where
    num = read <$> many1 digit

