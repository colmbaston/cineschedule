{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

import Data.Maybe
import Data.Char
import Data.List
import Data.Ord

import System.IO
import System.Directory
import System.IO.Unsafe
import System.Exit
import System.Process

import Parser

-- MAIN --

url :: String
url = "www.cineworld.co.uk/syndication/all-performances.xml"

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin  NoBuffering
          putStrLn ""
          (mc,md,r) <- getArgs >>= validateArgs (Nothing,Nothing,False)

          d <- case md of
                 Nothing -> T.putStrLn ("No day option provided - defaulting to " `T.append` defaultDay `T.append` "...") >> return (fromJust . getDate $ unpack defaultDay)
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
            Left err -> refresh absFile d c $ "Parse error - \"" ++ err ++ "\" - Refreshing file..."
            Right [] -> refresh absFile d c $ "No films found - Refreshing file..."
            Right fs -> if length fs < 10
                           then refresh absFile d c $ "Fewer than 10 films found - Refreshing file..."
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
                       Right [] -> putStrLn ("No films found - Did you enter the correct arguments?\n")               >> exitFailure
                       Right fs -> filmHandler (unpack c) d fs >> exitSuccess

download :: FilePath -> IO ()
download f = callProcess "wget" ["-q","--show-progress","-O",f,url]

type Film = (Text,Int,[(Int,Int)])

filmHandler :: String -> (Int,Int) -> [Film] -> IO ()
filmHandler c (d,m) fs = do let l  = length fs
                            let ll = zip [0..] (map (\(x,_,_) -> x) fs)
                            putStrLn ""
                            putStrLn ("Films showing at Cineworld " ++ up c ++ " on " ++ dpad d ++ "/" ++ dpad m ++ ":")
                            putStrLn ""
                            mapM_ (\(n,f) -> putStr (lpad (length (show l) + 1) (show n) ++ ": ") >> T.putStrLn f) ll
                            putStrLn ""
                            putStr "Enter the values of the films that you wish to schedule (space separated, press 'q' to exit): "
                            ns <- fmap nubsort $ getNumbers l
                            putStrLn ""
                            case schedule (select ns fs) of
                              ([],_) -> putStrLn "No possible schedulings!"
                              xs -> uncurry prettyPrint xs
                            putStrLn ""
  where
    up = unwords . map (\(x:xs) -> toUpper x : xs) . words
    dpad n    = (if n < 10 then ('0':) else id) (show n)
    lpad m xs = replicate (m - length ys) ' ' ++ ys
      where ys = take m xs


getLineUntilQ :: IO String
getLineUntilQ = do c <- getChar
                   case toLower c of
                     'q'  -> putStrLn "" >> exitSuccess
                     '\n' -> return [c]
                     x    -> do xs <- getLineUntilQ
                                return (x:xs)

getNumbers :: Int -> IO [Int]
getNumbers l = do ss <- fmap words getLineUntilQ
                  if all (all isDigit) ss
                     then do let ns = map read ss
                             if all (<l) ns
                                then return ns
                                else do putStr "One or more of your entries are out of range - Try again: "
                                        getNumbers l
                     else do putStr "Parse error - Try again: "
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

validateArgs :: (Maybe Text,Maybe (Int,Int),Bool) -> [String] -> IO (Maybe Text,Maybe (Int,Int),Bool)
validateArgs p [] = return p
validateArgs (mc,md,False)  ("-r":xs)   = validateArgs (mc,md,True) xs
validateArgs (Nothing,md,b) ("-c":x:xs) = validateArgs (Just $ pack x,md,b) xs
validateArgs (mc,Nothing,b) ("-d":x:xs) = case getDate (map toLower x) of
                                            Nothing -> usageFail
                                            jd      -> validateArgs (mc,jd,b) xs
validateArgs _ _ = usageFail

usageFail :: IO a
usageFail = putStrLn "Usage: cineschedule [-d day] [-c cinema]" >> exitFailure

defaultCinema :: Text
defaultCinema = "Nottingham"

defaultDay :: Text
defaultDay = "today"

getDate :: String -> Maybe (Int,Int)
getDate "today"    = getDate "0"
getDate "tomorrow" = getDate "1"
getDate s | all isDigit s = Just $ daysAhead (read s)
          | otherwise     = do w <- weekday s
                               let (_,_,w') = toWeekDate today
                               return . daysAhead $ (w + 7 - w') `mod` 7

today :: Day
today = utctDay $ unsafePerformIO getCurrentTime

daysAhead :: Int -> (Int,Int)
daysAhead n = (\(_,m,d) -> (d,m)) . toGregorian $ addDays (fromIntegral n) today

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
toTime24 = flip divMod 60

fromTime24 :: (Int,Int) -> Int
fromTime24 (h,m) | h == 0    = 1440 + m
                 | otherwise = h * 60 + m

showTime24 :: (Int,Int) -> Text
showTime24 (h,m) = (if h < 10 then T.cons '1' else id) (T.pack (show h)) `T.append` ":" `T.append` (if m < 10 then T.cons '1' else id) (T.pack (show m))

adjacents :: [a] -> [(a,a)]
adjacents xs = zip xs (tail xs)

intervals :: [(Text,Int,Int)] -> [Int]
intervals ss = zipWith (subtract) ls . map (uncurry subtract) $ adjacents ts
  where
    (_,ls,ts) = unzip3 ss


intervalsEnd :: [(Text,Int,Int)] -> ([Int],Int)
intervalsEnd ss = (zipWith (subtract) ls . map (uncurry subtract) $ adjacents ts, lastDuration + lastTime)
  where
    (_,ls,ts) = unzip3 ss
    (_,lastDuration,lastTime) = last ss

valid :: [(Text,Int,Int)] -> Bool
valid = all (>=0) . intervals

interleave :: [a] -> [a] -> [a]
interleave    []     []  = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- scheduling

schedule :: [Film] -> ([[(Text,(Int,Int))]],[([Int],Int)])
schedule fs = (map (map (\(x,_,z) -> (x,toTime24 z))) vs, map intervalsEnd vs)
  where
    (ts,ls,ss) = unzip3 fs

    vs :: [[(Text, Int, Int)]]
    vs = sortBy (comparing (Down . sum . intervals)) . filter valid . map (sortBy (comparing (\(_,_,x) -> x)) . zip3 ts ls . map fromTime24) $ sequence ss

-- IO

prettyPrint :: [[(Text,(Int,Int))]] -> [([Int],Int)] -> IO ()
prettyPrint ss = T.putStrLn . T.intercalate "\n\n\n" . map showOption . zip ss

showOption :: ([(Text,(Int,Int))],([Int],Int)) -> Text
showOption (xs,times) = T.intercalate "\n" $ interleave (map showFilm xs) (showIntervals times)
  where
    showFilm (f,(h,m)) = "  " `T.append` spad f `T.append` tpad h `T.append` ":" `T.append` tpad m
    tpad x = if x < 10 then '0' `T.cons` T.pack (show x) else T.pack (show x)
    spad f = f `T.append` ": " `T.append` T.replicate (l - T.length f) " "
    l = maximum $ map (T.length . fst) xs

    showIntervals :: ([Int],Int) -> [Text]
    showIntervals ([],et)     = ["  \ESC[1;32m" `T.append` "ends at " `T.append` showTime24 (toTime24 et) `T.append` "\ESC[0m"]
    showIntervals ((t:ts),et) = ("  \ESC[1;32m" `T.append` T.pack (show t) `T.append` " minute interval\ESC[0m") : showIntervals (ts,et)
