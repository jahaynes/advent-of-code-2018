import           Data.IntSet      (IntSet)
import qualified Data.IntSet as S

main :: IO ()
main = do
    input <- map read . lines . filter (/='+') <$> readFile "./day1_input"
    print (part1 input)
    print (part2 S.empty 0 (cycle input))

part1 :: [Int] -> Int
part1 = sum

part2 :: IntSet -> Int -> [Int] -> Int
part2 seen state (x:xs)
    | S.member state' seen = state'
    | otherwise = part2 (S.insert state' seen) state' xs
        where state' = state + x
