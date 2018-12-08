import Data.Char (toUpper)
import Data.Set  (Set, toList, fromList)

main :: IO ()
main = do
    input <- {- Trim trailing \n -} init <$> readFile "./day5_input"
    print $ part1 input
    print $ part2 input

part1 :: String -> Int
part1 = length . go []
    where
    go (a:b:cs) xs | react a b = go cs xs
    go       as [] = {- reverse -} as
    go   as (x:xs) = go (x:as) xs

    react :: Char -> Char -> Bool
    react a b = a /= b && toUpper a == toUpper b

part2 :: String -> Int
part2 input = minimum [ len | s <- toList (symbols input),
                              let i = filter (\c -> toUpper c /= s) input
                                  len = part1 i ]
    where
    symbols :: String -> Set Char
    symbols = fromList . map toUpper
