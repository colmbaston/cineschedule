{-# LANGUAGE OverloadedStrings #-}

module Parser (runParser) where

import Data.Attoparsec.Text

import qualified Data.Text as T
import           Data.Text (Text)

import Data.List
import Data.Char
import Data.Function

import Control.Monad

type Film = (Text,Int,[(Int,Int)])

runParser :: (Int,Int) -> Text -> Text -> Either String [Film]
runParser d c = parseOnly (xmlParse d c)

xmlParse :: (Int,Int) -> Text -> Parser [Film]
xmlParse d c = do takeTill isEndOfLine
                  skipSpace
                  string "<cinemas>"
                  skipSpace
                  concat <$> manyTill (cinemaParse d c) (string "</cinemas>")

cinemaParse :: (Int,Int) -> Text -> Parser [Film]
cinemaParse d c = do string "<cinema name=\"Cineworld "
                     x <- takeTill (=='\"')
                     char '\"'
                     takeTill (=='>')
                     char '>'
                     skipSpace
                     string "<films>"
                     skipSpace
                     fs <- concat <$> manyTill (filmParse d) (string "</films>")
                     skipSpace
                     string "</cinema>"
                     skipSpace
                     if T.map toLower x == T.map toLower c
                        then return . sortOn (\(x,_,_) -> x) $ filter (\(x,_,_) -> T.take 3 x /= "M4J") fs
                        else return []

filmParse :: (Int,Int) -> Parser [Film]
filmParse d = do string "<film "
                 manyTill (do takeTill (=='\"')
                              char '\"'
                              takeTill (=='\"')
                              char '\"'
                              skipSpace) (string "length=\"")
                 l <- decimal
                 string " mins\" title=\""
                 imax <- dimensions
                 t <- takeTill (=='\"')
                 char '\"'
                 takeTill (=='>')
                 char '>'
                 skipSpace
                 ss <- concat <$> manyTill (do s <- showsParse d
                                               skipSpace
                                               return s) (string "</film>")
                 skipSpace
                 let gs = groupBy ((==) `on` snd) $ sortOn snd ss
                 return $ map (\g -> (t `T.append` vid imax `T.append` subt (snd $ head g), l, map fst g)) gs

dimensions :: Parser (Bool,Bool)
dimensions = choice [(True,True)   <$ string "(IMAX 3-D) ",
                     (True,False)  <$ string "(IMAX) ",
                     (False,True)  <$ string "(3D) ",
                     (False,False) <$ string "(2D) ",
                     return (False,False)]

subt :: Bool -> Text
subt True  = " (Subtitles)"
subt False = ""

vid :: (Bool,Bool) -> Text
vid (True,True)   = " (IMAX 3D)"
vid (True,False)  = " (IMAX)"
vid (False,True)  = " (3D)"
vid (False,False) = ""

showsParse :: (Int,Int) -> Parser [((Int,Int),Bool)]
showsParse d = do string "<shows>"
                  skipSpace
                  concat <$> manyTill (showParse d) (string "</shows>")

showParse :: (Int,Int) -> Parser [((Int,Int),Bool)]
showParse d = do string "<show date=\""
                 takeTill (==' ')
                 char ' '
                 x <- decimal
                 char ' '
                 m <- choice [string "Jan" >> return 01,
                              string "Feb" >> return 02,
                              string "Mar" >> return 03,
                              string "Apr" >> return 04,
                              string "May" >> return 05,
                              string "Jun" >> return 06,
                              string "Jul" >> return 07,
                              string "Aug" >> return 08,
                              string "Sep" >> return 09,
                              string "Oct" >> return 10,
                              string "Nov" >> return 11,
                              string "Dec" >> return 12]
                 string "\" time=\""
                 t <- do h <- decimal
                         char ':'
                         m <- decimal
                         char '\"'
                         return (h,m)
                 videoParse
                 audioParse
                 sessionParse
                 s <- subtitleParse
                 string "/>"
                 skipSpace
                 if d == (x,m)
                    then return [(t,s)]
                    else return []

videoParse :: Parser ()
videoParse = choice [void $ string " videoType=\"imax 3d\"",
                     void $ string " videoType=\"3d\"",
                     void $ string " videoType=\"imax\"",
                     return ()]

audioParse :: Parser ()
audioParse = choice [void $ string " audioType=\"audio described\"",
                     return ()]

subtitleParse :: Parser Bool
subtitleParse = choice [True <$ string " subtitled=\"true\"",
                        return False]

sessionParse :: Parser ()
sessionParse = choice [void $ string " sessionType=\"dbox\"",
                       return ()]
