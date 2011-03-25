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
import Data.List

main = do
  --make dest, posts and static if not exist
  args <- getArgs
  curdir <- getCurrentDirectory
  forM (words "dest static posts") $ createDirectoryIfMissing False

  --sync static to dest
  system "cp static/* dest/"

  --find all posts
  posts <- fmap (filter (extIn ["ibug", "ihs"])) $ getDirectoryContents "posts"
  setCurrentDirectory "posts"
  forM_  posts $ \post -> do
      title <- postTitle post
      exists <- doesFileExist $ "../dest/"++urlify title++".html"
--      print (exists, "../dest/"++setExt "html" post) 
      if not exists || "-f" `elem` args 
         then genPost post
         else do
           tmHtml <- getModificationTime $ "../dest/"++urlify title++".html"
           tmSrc <-getModificationTime $ post
           when (tmSrc>tmHtml) $ genPost post
  --make index from posts list
  genIndex posts
  putStrLn $ "file://"++curdir++"dest/index.html"
  
  setCurrentDirectory curdir
  system "mv posts/*.png  dest/"
  return ()

genIndex posts = do 
  posts1 <- forM posts $ \post -> do
     title <- postTitle post
     para1 <- postFirstPara post
     tmSrc <-getModificationTime $ post
     return (title, urlify title++".html", para1, tmSrc) 
  writeFile "../dest/index.html" $ renderHtml $docTypeHtml $ do
     H.head $ do
         H.title "openbrain blog"
         link ! rel "stylesheet" ! href "style.css" ! type_ "text/css"
     body $ do
         p "openbrain blog"
         forM_ posts1 linkpost

linkpost (title,url,para1, tm) = do 
    H.div $ h2 $ a ! href (fromString url) $ toHtml title
    H.div $ em $ small $ fromString $ show tm
    p ""
    H.div $ fromString para1

genPost post | "ibug" `isSuffixOf` post = do 
     title <- postTitle post
     putStrLn $ "Generating "++post++"..."
     must $ system $ "bays --markdown "++post
     appendFile (setExt "md" post) =<< createBottom post
     must $ system $ "pandoc -V title-prefix=openbrain -c style.css -o "++setExt "html" post ++" "++setExt "md" post
     must $ system $ "mv "++setExt "html" post++" ../dest/"++urlify title++".html"
     must $ system $ "rm "++setExt "md" post
     must $ system $ "cp "++post++" ../dest/"
genPost post | "ihs" `isSuffixOf` post = do 
     title <- postTitle post
     putStrLn $ "Generating "++post++"..."
     must $ system $ "inlit --nopandoc "++post
     appendFile (setExt "md" post) =<< createBottom post
     must $ system $ "pandoc -V title-prefix=openbrain -c style.css -o "++setExt "html" post ++" "++setExt "md" post
     must $ system $ "mv "++setExt "html" post++" ../dest/"++urlify title++".html"
     must $ system $ "rm "++setExt "md" post
     must $ system $ "cp "++post++" ../dest/"


postTitle post = 
  (drop 2 . P.head . lines) `fmap` readFile post
postFirstPara post = 
    (unlines . takeWhile para . dropWhile hdr . lines) `fmap` readFile post
 where hdr ('%':s) = True
       hdr [] = True
       hdr ('>':_) = True
       hdr _ = False
       para ('>':_) = False 
       para ('?':'>':_) = False
       para s = True

createBottom :: String -> IO String
createBottom post = do
  tmSrc <-getModificationTime post
  let originalLink = case () of
           _ |  "ibug" `isSuffixOf` post 
                -> concat ["Created from an inliterate [baysig]",
                           "(http://github.com/glutamate/baysig/) module: [",
                           post, "](", post, ")"]
           _ |  "ihs" `isSuffixOf` post 
                -> concat ["Created from an [inliterate]",
                           "(http://github.com/glutamate/inliterate/) ",
                           "[Haskell](http://haskell.org) module: [",
                           post, "](", post, ")"]
  return $ "\n\n"++show tmSrc++"\n\n"++originalLink

--postTitle post = 
--  (drop 2 . P.head . lines) `fmap` readFile post
  

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
-- links from posts to index, prev/next posts
-- sort front page

-}