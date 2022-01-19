{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

import Data.Maybe
import Data.Char
import Data.List
import Data.Ord

import System.IO
import System.Directory
import System.Exit
import System.Process

import System.Console.Haskeline

import Parser

-- MAIN --

url :: String
url = "https://www.cineworld.co.uk/syndication/all-performances.xml"

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn ""
          (mc,md,r) <- getArgs >>= validateArgs (Nothing,Nothing,False)

          today <- getToday

          d <- case md of
                 Nothing -> T.putStrLn ("No day option provided - defaulting to " `T.append` defaultDay `T.append` "...") >> return (fromJust (getDate (unpack defaultDay) today))
                 Just  d -> return d

          c <- case mc of
                 Nothing -> T.putStrLn ("No cinema option provided - defaulting to " `T.append` defaultCinema `T.append` "...") >> return defaultCinema
                 Just  c -> return c

          absDir <- (++ "/.cache") <$> getHomeDirectory
          b <- doesDirectoryExist absDir
          unless b (createDirectory absDir)

          let absFile = absDir ++ "/cineschedule.xml"
          b <- doesFileExist absFile

          if r
            then refresh absFile d c "Refresh option enabled - Refreshing file..."
            else unless b (refresh absFile d c "File not present - Downloading...")
          putStrLn "Parsing file..."
          t <- T.readFile absFile
          case runParser d c t of
            Left err -> refresh absFile d c ("Parse error - \"" ++ err ++ "\" - Refreshing file...")
            Right [] -> refresh absFile d c "No films found - Refreshing file..."
            Right fs -> if length fs < 10
                           then refresh absFile d c "Fewer than 10 films found - Refreshing file..."
                           else filmHandler (unpack c) d fs

refresh :: FilePath -> (Int,Int) -> Text -> String -> IO ()
refresh f d c s = do putStrLn s
                     putStrLn ""
                     download f
                     putStrLn ""
                     putStrLn "Parsing file..."
                     t <- T.readFile f
                     case runParser d c t of
                       Left err -> putStrLn ("Parse error - \"" ++ err ++ "\" - The file format may have changed!\n") >> exitFailure
                       Right [] -> putStrLn  "No films found - Did you enter the correct arguments?\n"             >> exitFailure
                       Right fs -> filmHandler (unpack c) d fs >> exitSuccess

download :: FilePath -> IO ()
download f = callProcess "wget" ["-q", "--show-progress", "-O", f, url]

filmHandler :: String -> (Int,Int) -> [Film] -> IO ()
filmHandler c (d,m) fs = do let l  = length fs
                            let ll = zip [0..] (map (\(x,_,_) -> x) fs)
                            putStrLn ""
                            putStrLn ("Films showing at Cineworld " ++ up c ++ " on " ++ dpad d ++ "/" ++ dpad m ++ ":")
                            putStrLn ""
                            mapM_ (\(n,f) -> putStr (lpad (length (show l) + 1) (show n) ++ ": ") >> T.putStrLn f) ll
                            putStrLn ""
                            putStr "Enter the values of the films that you wish to schedule (space separated, press 'q' to exit): "
                            ns <- nubsort <$> getNumbers l
                            case schedule (select ns fs) of
                               [] -> putStrLn "No possible schedulings!"
                               os -> do let (ns,_,_) = unzip3 (head os)
                                        mapM_ (\x -> putChar '\n' >> printOption (maximum (map T.length ns)) x >> putChar '\n') os

  where
    up = unwords . map (\(x:xs) -> toUpper x : xs) . words
    dpad n    = (if n < 10 then ('0':) else id) (show n)
    lpad m xs = replicate (m - length ys) ' ' ++ ys
      where ys = take m xs

getNumbers :: Int -> IO [Int]
getNumbers l = do mss <- fmap words <$> runInputT (Settings noCompletion Nothing False) (getInputLine "")
                  case mss of
                    Nothing -> do putStr "Input error - Try again: "
                                  getNumbers l
                    Just ss -> if | all (all isDigit) ss               -> do let ns = map read ss
                                                                             if all (<l) ns
                                                                               then return ns
                                                                               else do putStr "One or more of your entries are out of range - Try again: "
                                                                                       getNumbers l
                                  | any (\x -> map toLower x == "q") ss ->    exitSuccess
                                  | otherwise                           -> do putStr "Parse error - Try again: "
                                                                              getNumbers l

nubsort :: Ord a => [a] -> [a]
nubsort = map head . group . sort

select :: [Int] -> [Film] -> [Film]
select = go 0
  where
    go _ [] _      = []
    go n (x:xs) (f:fs) | x == n = f : go (n+1) xs fs
                       | otherwise  = go (n+1) (x:xs) fs

-- ARGUMENT HANDLING --


defaultCinema :: Text
defaultCinema = "Nottingham"

defaultDay :: Text
defaultDay = "today"

validateArgs :: (Maybe Text,Maybe (Int,Int),Bool) -> [String] -> IO (Maybe Text,Maybe (Int,Int),Bool)
validateArgs p [] = return p
validateArgs (mc,md,False)  ("-r":xs)   = validateArgs (mc,md,True) xs
validateArgs (Nothing,md,b) ("-c":x:xs) = validateArgs (Just $ pack x,md,b) xs
validateArgs (mc,Nothing,b) ("-d":x:xs) = do d <- getToday
                                             case getDate (map toLower x) d of
                                               Nothing -> usageFail
                                               jd      -> validateArgs (mc,jd,b) xs
validateArgs _ _ = usageFail

getToday :: IO Day
getToday = adjust <$> getCurrentTimeZone <*> getCurrentTime
  where
    adjust :: TimeZone -> UTCTime -> Day
    adjust z = utctDay . addUTCTime (fromIntegral (timeZoneMinutes z) * 60)

getDate :: String -> Day -> Maybe (Int,Int)
getDate "today"    d = getDate "0" d
getDate "tomorrow" d = getDate "1" d
getDate s d | all isDigit s = Just $ daysAhead (read s) d
            | otherwise     = do w <- weekday s
                                 let (_,_,w') = toWeekDate d
                                 pure (daysAhead ((w + 7 - w') `mod` 7) d)

daysAhead :: Int -> Day -> (Int,Int)
daysAhead n today = (\(_,m,d) -> (d,m)) . toGregorian $ addDays (fromIntegral n) today

usageFail :: IO a
usageFail = putStrLn "Usage: cineschedule [-c cinema] [-d day] [-r]" >> exitFailure

weekday :: String -> Maybe Int
weekday "monday"    = Just 1
weekday "tuesday"   = Just 2
weekday "wednesday" = Just 3
weekday "thursday"  = Just 4
weekday "friday"    = Just 5
weekday "saturday"  = Just 6
weekday "sunday"    = Just 7
weekday _ = Nothing

--------------------------------------

toTime24 :: Int -> (Int,Int)
toTime24 n = (h `mod` 24, m)
  where
    (h,m) = divMod n 60

fromTime24 :: (Int,Int) -> Int
fromTime24 (h,m) | h == 0    = 1440 + m
                 | otherwise = h * 60 + m

showTime24 :: (Int,Int) -> Text
showTime24 (h,m) = (if h < 10 then T.cons '0' else id) (T.pack (show h)) `T.append` ":" `T.append` (if m < 10 then T.cons '0' else id) (T.pack (show m))

printMinutes :: Int -> IO ()
printMinutes m = do putStr (show m)
                    putStr " minute"
                    unless (m == 1) (putChar 's')

-- scheduling

type Film   = (Text,Int,[(Int,Int)])
type Option = [(Text, Int, (Int,Int))]

adjacents :: [a] -> [(a,a)]
adjacents = zip <*> tail

schedule :: [Film] -> [Option]
schedule fs = (map fst . sortOn (Down . sum . snd) . filter (all (>= 0) . snd)) (zip os iss)
  where
    (ts,ls,ss) = unzip3 fs

    os :: [Option]
    os = map (sortOn (\(_,_,z) -> fromTime24 z) . zip3 ts ls) (sequence ss)

    iss :: [[Int]]
    iss = map intervals os

intervals :: Option -> [Int]
intervals o = zipWith (\l (t1,t2) -> t2 - t1 - l) ls (adjacents (map fromTime24 ts))
  where
    (_,ls,ts) = unzip3 o

-- IO

printOption :: Int -> Option -> IO ()
printOption p o = do putStr "  Total interval duration: "
                     printMinutes (sum (intervals o))
                     putChar '\n'
                     go o
  where
    go []           = pure ()
    go ((n,l,t):os) = do T.putStr "    "
                         T.putStr n
                         T.putStr ": "
                         T.putStr (T.replicate (p - T.length n) " ")
                         T.putStr (showTime24 t)
                         T.putStr " \ESC[1;32m(end time: "
                         let endTime = l + fromTime24 t
                         T.putStr (showTime24 (toTime24 endTime))
                         case os of
                           []         -> pure ()
                           (_,_,t'):_ -> do T.putStr ", interval duration: "
                                            printMinutes (fromTime24 t' - endTime)
                         T.putStrLn ")\ESC[0m"
                         go os
