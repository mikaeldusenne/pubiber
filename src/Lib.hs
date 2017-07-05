{-# LANGUAGE OverloadedStrings #-}
module Lib where

import General (just_or_default,fromJust,(!∫),alphaUpper)
import List
import Tuple(applyToFst, uncurry3)
import Control.Monad(join)
import Parser(many)
import Text.Read(readMaybe)
import Network.HTTP.Client
import Data.ByteString.Lazy.Char8(unpack,pack)
import Requester

---- Utils ----

nomaybe = just_or_default "(x)"
notempty = (const "(x)" !∫ id) . isEmpty


---- Data ----

data Medline = ML {header::String, content::String}

instance Read Medline where
  readsPrec _ r = [ (ML h c,xs) | (h,s) <- lex r,
                    ("-",s') <- lex s,
                    (c,xs) <- rb [] s' ]
    where rh = (:[]) . span (`elem`alphaUpper)
          rb acc [] = [(acc,"")]
          rb acc s  = if beginWith " " l
                      then (rb (acc++f acc l) ls)
                      else [(acc,s)]
            where (l,ls) = f . span (/='\n') $ s
                    where f (a,[]) = (a,[])
                          f (a,(x:xs)) = (a,xs)
                  f e = (if e==[] then id else (' ':))
                    . dropWhile (==' ')

instance Show Medline where
  show (ML h c) = h ++ " ~> " ++ c

---- constants ----

-- pubmed_fetch_id s = exec "efetch" .
--   append s . words $ "-db pubmed -format medline -id"

elements :: [(String, [Medline] -> Maybe String)]
elements = uncurry zip
  . applyToFst allToMaxSize . unzip . map f $ es
  where f (a,b,c) = (a, c b)
        -- type F = String -> [Medline] -> Maybe String
        -- es :: [(String,String,F)]
        es = [("author"   ,"FAU", ((concatWith " and " <$>) .) . findAll),
              ("title"    ,"TI",  find),
              ("publisher","JT",  find),
              ("date"     ,"DP",  ((>>=parseDate).).find)]

---- Functions ----

-- read one article written as a list of "-format medline" fetched ids
-- readMLs :: String -> [Medline]
readMLs = fst . head . many (reads::ReadS Medline)


-- findAll :: String -> [Medline] -> Maybe [String]
findAll h = nothingIf isEmpty . (content <$>)
  . filter ((==h).header)

-- find :: String -> [Medline] -> Maybe String
find h = join . (safe_head<$>) . findAll h


-- parseDate :: String -> Maybe String
parseDate = safe_head . filter isY . words
  where isY s = and [i/=Nothing,
                     (>1000)$fromJust i]
          where i = (readMaybe::String-> Maybe Int) s
          

-- tobibnick :: String -> [Medline] -> String
tobibnick nick comment l = unlines
  . surround2 [first] ["}"]
  . map  ("    "++)
  . add "keywords" "trusted"
  . add "comment" comment
  . (fromJust<$>) . filter (/=Nothing) . map f $ elements
  where 
        first = "@article{" ++nick++","
        f (a,b) =  ((a ++) . curly<$>) . b $ l
        curly = surround2 "={" "},"
        add a = append . (s a++) . curly
          where s = fix'size size ' '
                  where size=count.fst.head$elements

tobib c l = (\n -> tobibnick n c l) . nomaybe . find "PMID" $ l
             
pubmed_fetch_id = (>>=send).build POST fetchurl
  . RequestBodyLBS . pack . ("rettype=medline&db=pubmed&id="++)
  where fetchurl = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
    

-- :: [String] -> String -> [String]
request'result'to'biber nicks comment =
  map (uncurry3 tobibnick)
  . zip3 nicks comment -- 
  . map (readMLs.unlines) -- parse each record
  .splitOn (==[]).lines -- separate each record (empty line)


file_to_bib :: String -> IO [String]
file_to_bib path =
  ((transpose.map (splitOn (=='|')).lines) <$> readFile path)
  >>= (\[ids,nicks,comment] -> (pubmed_fetch_id $ concatWith "," ids)
        >>= ( return . request'result'to'biber nicks comment))
