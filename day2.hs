import qualified Data.Map as M

newtype BoxId =
    BoxId String deriving (Eq, Ord)

newtype Checksum =
    Checksum Int deriving Show

newtype Distance =
    Distance Int deriving Eq

data ChecksumInput = ChecksumInput
                   { twice  :: Bool
                   , thrice :: Bool }

main :: IO ()
main = do

    input <- map BoxId . lines <$> readFile "./day2_input"

    print (part1 input)

    print (part2 input)

part1 :: [BoxId] -> Checksum
part1 = checkSum . map scanBox

    where
    scanBox :: BoxId -> ChecksumInput
    scanBox (BoxId boxId) =
        let counts = map snd . M.toList . M.fromListWith (+) . zip boxId $ repeat (1::Int)
        in
        ChecksumInput { twice  = any (==2) counts
                      , thrice = any (==3) counts }

    checkSum :: [ChecksumInput] -> Checksum
    checkSum checksumInputs =
        let twices = length . filter id . map twice $ checksumInputs
            thrices = length . filter id . map thrice $ checksumInputs
        in
        Checksum (twices * thrices)

part2 :: [BoxId] -> [String]
part2 input = 
    [ common a b | a <- input,
                   b <- input,
                   distance a b == Distance 1,
                   a <= b ]

    where
    distance :: BoxId -> BoxId -> Distance
    distance (BoxId a) (BoxId b) = Distance . length . filter not $ zipWith (==) a b

    common :: BoxId -> BoxId -> String
    common (BoxId a) (BoxId b) = concat $ zipWith (\x y -> if x == y then [x] else []) a b
