{-# LANGUAGE ApplicativeDo #-}
module Main where

import System.Environment
import Lib
import List
import Misc (orelse)

import Control.Monad
import Data.Foldable (traverse_)

import System.FilePath.Posix
import System.Directory
import Options.Applicative
import qualified Options.Applicative as O (header)

import Data.Semigroup ((<>))


data SettingsOpts = SettingsOpts {
  dir :: String,
  from :: String,
  to :: String,
  manual :: String
  }
  deriving (Show, Read)


opts :: Parser SettingsOpts
opts = SettingsOpts
  <$> argument str (metavar "DIR")
  <*> f "from" 'i' "txt file with PMIDs"           "references.txt"
  <*> f "to"   'o' ".bib file to generate"         "references.bib"
  <*> f "also" '+' ".bib file to append to output" "manual.bib"
  where f l s h v = strOption $ long l <> short s <> help h <> value v

parseFile :: String -> IO [[String]]
parseFile path =
  let f = map (\(ML k v) -> v)
  in (transpose.map f.readArticles . stripcomments <$> readFile path)

main :: IO ()
main = do
  let soptsP = info (opts <**> helper)
        (fullDesc <> progDesc "generate bibtex from PMIDs")
  (SettingsOpts{dir=dir, from=from, to=to, manual=manual}) <- execParser soptsP


  [ids,nicks,comment] <- parseFile from

  putStrLn $ show (length ids) ++ " articles to fetch."
  
  let createNote :: FilePath -> String -> IO ()
      createNote name content = (>>= mempty `orelse` writeFile f content) . doesFileExist $ f
        where f = (dir </> "notes" </> name <.> "org")
        
  doesDirectoryExist (dir </> "notes") >>= (`when` traverse_ (uncurry createNote) (zip nicks comment))

  let bibz = ((dropWhile (=='\n')<$>) . pubmed_fetch_id $ concatWith "," ids)
             >>= ( return . request'result'to'biber nicks comment)
  
  pubmed_bibs <- concatWith "\n\n" <$> bibz
  manual_bibs <- doesFileExist manual >>= (readFile manual
                                                  `orelse` mempty)
  writeFile to $ pubmed_bibs <> "\n\n" <> manual_bibs
  -- >>= (\(from:to:_) ->
  --         >>=  writeFile to )
  putStrLn "ok"


-- main = getArgs
--   >>= (\(from:to:_) ->
--          concatWith "\n\n" <$> file_to_bib from >>=  writeFile to )

