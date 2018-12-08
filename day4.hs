import           Data.List           (sort, maximumBy, foldl')
import           Data.Map            (Map)
import qualified Data.Map as M
import           Data.Ord            (comparing)
import           Data.Time
import           Data.Time.LocalTime (diffLocalTime)
import           Text.Parsec

newtype GuardId = GuardId Int deriving (Ord, Eq, Show)

data Action = BeginShift GuardId
            | FallAsleep
            | Wakeup
                deriving (Ord, Eq, Show)

data Row = Row LocalTime Action
               deriving (Ord, Eq, Show)

type Min = Int

main :: IO ()
main = do

  strInput <- readFile "./day4_input"

  case parse parseSchedule "day4" strInput of
    Left parseError -> error (show parseError)
    Right schedule  -> do
        part1 schedule
        part2 schedule

part1 :: [Row] -> IO ()
part1 schedule = do
    let sleepyGuard@(GuardId gi) = mostSleptGuard schedule
    let msm = mostSleptMinute sleepyGuard schedule
    print $ gi * msm

    where
    mostSleptMinute :: GuardId -> [Row] -> Min
    mostSleptMinute g = go False M.empty
        where
        go   _ acc [] = fst . maximumBy (comparing snd) . M.toList $ acc
        go   _ acc (Row _ (BeginShift sg):rs) = go (sg == g) acc rs
        go True acc (Row sleepTime FallAsleep:Row wakeTime Wakeup:rs) = do
            let sleepyMinutes = init [todMin . localTimeOfDay $ sleepTime .. todMin . localTimeOfDay $ wakeTime]
            go True (foldl' (\mp mn -> M.insertWith (+) mn (1::Int) mp) acc sleepyMinutes) rs
        go rg acc (_:rs) = go rg acc rs

    mostSleptGuard :: [Row] -> GuardId
    mostSleptGuard = go undefined M.empty
        where
        go _ countMins [] = fst . maximumBy (comparing snd) . M.toList $ countMins
        go _ countMins (Row _ (BeginShift g):rs) = go g (M.insertWith (+) g (0::Int) countMins) rs
        go gid countMins (Row sleepTime FallAsleep:Row wakeTime Wakeup:rs) = do
            let mins = round $ (nominalDiffTimeToSeconds $ diffLocalTime wakeTime sleepTime) / 60.0
            go gid (M.insertWith (+) gid mins countMins) rs
        go _ _ _ = undefined

part2 :: [Row] -> IO ()
part2 schedule = do
    let (m, GuardId g, _) = go undefined M.empty schedule
    print $ m * g

    where
    go _ acc [] = unpack acc
    go _ acc (Row _ (BeginShift g):rs) = go g acc rs
    go gid acc (Row sleepTime FallAsleep:Row wakeTime Wakeup:rs) = do
        let sleepyMinutes = init [todMin . localTimeOfDay $ sleepTime .. todMin . localTimeOfDay $ wakeTime]
        let acc' = foldl' (ins gid) acc sleepyMinutes
        go gid acc' rs
    go _ _ _ = undefined

    unpack acc = do
        let unpacked = concatMap (\(a,ls) -> map (\(k,v) -> (a,k,v)) ls ) $ M.toList (M.toList <$> acc)
        maximumBy (comparing (\(_,_,c) -> c)) $ unpacked

    ins :: GuardId -> Map Min (Map GuardId Int) -> Min -> Map Min (Map GuardId Int)
    ins gid acc mn = M.insertWith f mn (M.singleton gid 1) acc

        where
        f :: Map GuardId Int -> Map GuardId Int -> Map GuardId Int
        f a b = M.fromListWith (+) (M.toList a ++ M.toList b)

parseSchedule :: Parsec String t [Row]
parseSchedule = sort <$> many parseRow

  where
  parseRow = Row <$> parseDate   <* char ' '
                 <*> parseAction <* endOfLine

    where
    parseDate = do
        strTime <- char '[' *>  many1 (noneOf "]") <* char ']'
        case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" strTime of
            Just localTime -> pure localTime
            Nothing        -> error "Could not parse time"

    parseAction :: Parsec String t Action
    parseAction = try beginShift
              <|> try fallAsleep
              <|> wakeUp

        where
        fallAsleep = const FallAsleep <$>  string "falls asleep"
        wakeUp     = const Wakeup     <$>  string "wakes up"
        beginShift =       BeginShift <$> (string "Guard #" *> (GuardId <$> num) <* string " begins shift")

num :: Parsec String t Int
num = read <$> many1 digit
