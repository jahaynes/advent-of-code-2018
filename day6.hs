import Data.Monoid     ((<>))
import Data.List       (sortOn, groupBy, partition, maximumBy, minimumBy)
import Data.List.Split (splitOn)
import Data.Ord        (comparing)
import Data.Set        (Set)
import Data.Map        (Map)
import qualified Data.Set as S
import qualified Data.Map as M

data NamedCoord = NamedCoord { name :: !Char
                             , ncx  :: !Int
                             , ncy  :: !Int
                             } deriving (Eq, Ord, Show)

data Coord = Coord !Int !Int
                 deriving (Eq, Ord, Show)

newtype Name = Name Char
                   deriving Show

parse :: (String, Name) -> NamedCoord
parse (s, Name n) = case splitOn ", " s of
                        [x,y] -> NamedCoord n (read x) (read y)
                        _     -> error "Bad data"

buildMap :: [NamedCoord] -> Map Coord Char
buildMap = M.fromList . map (\(NamedCoord n x y) -> (Coord x y, n))


initialFringe :: [String] -> Fringe
initialFringe ls = map parse
                 . zip ls
                 $ map Name ['A'..]

type Fringe = [NamedCoord]
type Done   = Map Coord Char
type Escaped = Set Char
data Space  = Space Fringe Done Escaped deriving Show

expand :: Int -> Int -> Int -> Int -> NamedCoord -> ([NamedCoord], Escaped)
expand minX minY maxX maxY (NamedCoord n x y) =

    let (expanded, escaped) = partition (\(NamedCoord _ x' y') -> x' < minX
                                                               || x' > maxX + 1
                                                               || y' < minY
                                                               || y' > maxY + 1)
                              [ NamedCoord n (x-1)     y
                              , NamedCoord n     x (y-1)
                              , NamedCoord n (x+1)     y
                              , NamedCoord n     x (y+1) ]

    in (escaped, S.fromList $ map name expanded)

markEquidistant :: [NamedCoord] -> [NamedCoord]
markEquidistant ns | length ns < 2 = ns
                   | otherwise     = map (\(NamedCoord _ x y) -> NamedCoord '.' x y) ns

explore :: Int -> Int -> Int -> Int -> Space -> Space
explore minX minY maxX maxY = go
    where
    go s@(Space      []    _       _) = s
    go   (Space  fringe done escaped) = do

        -- The last fringe is baked into the map
        let done' = done <> buildMap fringe

        -- Create the new fringe from the old, noting any 'escapes'
        let expandedEscaped = map (expand minX minY maxX maxY) fringe

        let escaped' = mconcat (escaped:map snd expandedEscaped)

        let expanded = S.toList
                     . S.fromList 
                     . filter (\(NamedCoord _ x y) -> not $ M.member (Coord x y) done')
                     . concatMap fst
                     $ expandedEscaped

        let fringe' = concatMap markEquidistant
                    . groupBy (\n1 n2 -> ncx n1 == ncx n2 && ncy n1 == ncy n2)
                    . sortOn (\(NamedCoord _ x y) -> (x,y))
                    $ expanded

        go (Space fringe' done' escaped')

dump :: [NamedCoord] -> IO ()
dump = mapM_ putStrLn
     . map (map name)
     . groupBy (\n1 n2 -> ncy n1 == ncy n2)
     . sortOn ncy

main :: IO ()
main = do

    ls <- lines <$> readFile "./day6_input"

    let fringe = initialFringe ls

    let minX = ncx . minimumBy (comparing ncx) $ fringe 
        minY = ncy . minimumBy (comparing ncy) $ fringe
        maxX = ncx . maximumBy (comparing ncx) $ fringe
        maxY = ncy . maximumBy (comparing ncy) $ fringe

    let (Space [] done escaped) = explore minX minY maxX maxY (Space fringe M.empty S.empty)

    let namedCoordinates = map (\(Coord x y,c) -> NamedCoord c x y) $ M.toList done

    let maxFinite = maximumBy (comparing snd)
                  . M.toList
                  . M.fromListWith (+)
                  . map (\(a,b) -> (b,a))
                  . zip (repeat (1::Int))
                  . map name
                  . filter (\nc -> not (S.member (name nc) escaped))
                  $ namedCoordinates

    print maxFinite
