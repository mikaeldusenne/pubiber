{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Misc -- (just_or_default,fromJust,(!∫),(?),alphaUpper)
import List
import Tuple
import Control.Monad(join)
import Text.Read(readMaybe)
import Network.HTTP.Client
import System.Directory(doesFileExist)
import Data.ByteString.Lazy.Char8(unpack,pack)
import Requester
import System.FilePath.Posix
import ParsecArticle
import Text.Parsec (parse)
import Text.BibTeX.Parse
import Text.BibTeX.Entry

---- Utils ----

months = words $ "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"

many :: ReadS α -> ReadS [α]
many read = just_or_default []
  . nothingIf (isEmpty.fst.head)
  . f []
  where f acc [] = [(acc,"")]
        f acc r  = case read r of [] -> [(acc,r)]
                                  [(e,s)] -> f (acc++[e]) s


fromReads = (>>= (\(a,b) -> if isEmpty b
                            then Just a
                            else Nothing))
            . safe_head 


nomaybe = just_or_default "(x)"
notempty = (const "(x)" !∫ id) . isEmpty

---- Data ----

data Medline = ML {header::String, content::String}

instance Read Medline where
  readsPrec _ r = [ (ML h c,xs) | (h,s) <- rh r,
                    ("-",s') <- lex s,
                    (c,xs) <- rb [] s' ]
                  
    where rh = (:[]) . span (`elem`alphaUpper)
          rb acc [] = [(acc,"")]
          rb acc s  = if beginWith " " l
                      then (rb (acc++f acc l) ls)
                      else [(acc,s)]
            where (l,ls) = sliceOn '\n' $ s
                  f e = (if e==[] then id else (' ':))
                    . dropWhile (==' ')

instance Show Medline where
  show (ML h c) = h ++ " ~> " ++ c

-- data Article = Article {
--   nickname :: String,
--   author   :: String,
--   title    :: String,
--   publisher:: String,
--   journal  :: String,
--   date     :: String,
--   month    :: String,
--   year     :: String,
--   comment  :: String,
--   keywords :: String
--   }

---- constants ----

path = "/home/mika/Documents/random/sante_publique/cismef/article_pubmed_search_optimization/bibliographie"

-- pubmed_fetch_id s = exec "efetch" .
--   append s . words $ "-db pubmed -format medline -id"

elements :: [(String, [Medline] -> Maybe String)]
elements = uncurry zip
  . applyToFst (allToMaxSize ' ') . unzip
  . map f $ es
  where f (a,b,c) = (a, c b)
        -- type F = String -> [Medline] -> Maybe String
        -- es :: [(String,String,F)]
        es = [("author"   ,"FAU", ((concatWith " and " <$>) .) . findAll),
              ("title"    ,"TI",  find),
              ("publisher","JT",  find),
              ("journal"  ,"TA",  find),
              ("date"     ,"DP",  ((>>=parseDate).).find),
              ("month"    ,"DP",  ((>>=parseMonth).).find),
              ("year"    ,"DP",  ((>>=parseYear).).find)
             ]

---- Functions ----

-- read one article written as a list of "-format medline" fetched ids
-- readMLs :: String -> [Medline]
readMLs = fst . head . many (reads::ReadS Medline)

readsArticle :: ReadS [Medline]
readsArticle = map f . many (reads :: ReadS Medline)
  where f = applyToSnd $ dropWhile (=='\n') 

readArticles :: String -> [[Medline]]
readArticles = fromJust . fromReads . many readsArticle

-- findAll :: String -> [Medline] -> Maybe [String]
findAll h = nothingIf isEmpty . (content <$>)
  . filter ((==h).header)

-- find :: String -> [Medline] -> Maybe String
find h = join . (safe_head<$>) . findAll h
find' h = fromJust . (head<$>) . findAll h




-- parseDate :: String -> Maybe String
-- parseDate = safe_head . filter isY . words
--   where isY s = and [i/=Nothing,
--                      (>1000)$fromJust i]
--           where i = (readMaybe::String-> Maybe Int) s

-- https://www.nlm.nih.gov/bsd/mms/medlineelements.html#dp
parseDate = safe_head . words

parseMonth = (>>=safe_head) . safe_tail . words

parseYear = safe_head . words


getTitles :: String -> [String]
getTitles s = (map (snd . head . filter ((=="title") . fst) . fields) . fromEither) $ parse file "" s

-- tobibnick :: String -> [Medline] -> String
tobibnick nick comment l = unlines
  . surround2 [first] ["}"]
  . map  ("    "++)
  . add "keywords" "trusted"
  . add "comment" comment
  . (fromJust<$>) . filter isJust . map f $ elements
  where 
        first = "@article{" ++nick++","
        f (a,b) =  ((a ++) . curly<$>) . b $ l
        curly = surround2 "={" "},"
        add a = append . (s a++) . curly
          where s = fix'size size ' '
                  where size=count.fst.head$elements

tobib c l = (\n -> tobibnick n c l) . nomaybe . find "PMID" $ l

pubmed_fetch_url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

pubmed_default_args :: String
pubmed_default_args = concatWith "&" $ map (\(a, b) -> a ++ "=" ++ b) [
          ("db", "pubmed"),
          ("rettype", "medline")
          ]


pubmed_fetch_id :: String -> IO String
pubmed_fetch_id = (>>=send).build POST pubmed_fetch_url
  . RequestBodyLBS . pack . ( pubmed_default_args ++ ) . ("&id="++)


-- load_id id = doesFileExist f >>= ((readBibtex <$> readFile f) `orelse` pubmed_fetch_id id) 
--   where f = "/home/mika/.cache/pubiber" </> id

request'result'to'biber :: [String] ->  [String] -> String -> [String]
request'result'to'biber nicks comment =
  map (uncurry3 tobibnick)
  . zip3 nicks comment 
  . map readMLs
  . splitWhen "\n\n" -- separate each record (empty line)

stripcomments = unlines.filter (not.(==Just '#').safe_head) .lines


