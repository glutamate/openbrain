{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.Directory
import System.Cmd
import System.Exit
import System.Environment

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String

import Prelude as P

import Data.String
import Data.Char

main = do
  --make dest, posts and static if not exist
  args <- getArgs
  curdir <- getCurrentDirectory
  forM (words "dest static posts") $ createDirectoryIfMissing False

  --sync static to dest
  system "cp static/* dest/"

  --find all posts
  posts <- fmap (filter (extIn ["ibug"])) $ getDirectoryContents "posts"
  setCurrentDirectory "posts"
  forM_  posts $ \post -> do
      exists <- doesFileExist $ "../dest/"++setExt "html" post
      if not exists || "-f" `elem` args 
         then genPost post
         else do
           tmHtml <- getModificationTime $  "../dest/"++setExt "html" post
           tmSrc <-getModificationTime $ post
           when (tmSrc>tmHtml) $ genPost post
  --make index from posts list
  postsWithTitles <- forM posts $ \post -> do
     title <- postTitle post
     return (title, urlify title++".html")
  setCurrentDirectory curdir
  writeFile "dest/index.html" $ renderHtml $ genIndex postsWithTitles
  return ()

genIndex posts = docTypeHtml $ do
     H.head $ do
         H.title "Natural numbers"
     body $ do
         p "A list of natural numbers:"
         ul $ forM_ posts linkpost

linkpost (title,url) = li $ a ! href (fromString url) $ toHtml title

genPost post = do 
     title <- postTitle post
     putStrLn $ "Generating "++post++"..."
     must $ system $ "bays --markdown -pngdir../dest "++post
     must $ system $ "pandoc -V title-prefix=openbrain -c style.css -o "++setExt "html" post ++" "++setExt "md" post
     must $ system $ "cp "++setExt "html" post++" ../dest/"++urlify title++".html"


postTitle post = 
  (drop 2 . P.head . lines) `fmap` readFile post
  

urlify = P.map (toLower . spcsToHyph) where
  spcsToHyph ' ' = '-'
  spcsToHyph c = c

noExt = takeWhile (/='.')
getExt = drop 1 . dropWhile (/='.')
setExt ex = (++('.':ex)) . noExt


extIn exts fnm = (getExt fnm) `elem` exts 

must :: IO ExitCode -> IO ()
must ioe = do ExitSuccess <- ioe
              return ()

whenM mb mx = do
    b <- mb
    when b mx

{- TODO

-- adjustable template for posts
-- links from posts to index
-- index has first paragraph
-- posts html title
-- sensible urls
-- link to ibug file, copy it to dest too
-- also 


-}