module Feed where

import System.IO
import Control.Monad

genFeed posts h = do
  hPutStrLn h "<?xml version=\"1.0\" ?>\n<rss version=\"2.0\">"
  hPutStrLn h "<channel>"
  hPutStrLn h "<title>openbrain blog</title>"
  hPutStrLn h "<link>http://blog.openbrain.org</link>"
  hPutStrLn h "<description>openbrain blog</description>"
  forM_ posts $ \(title,url,para1, _, tm) -> do
     hPutStrLn h $ "<item>"
     hPutStrLn h $ "<title>"++title++"</title>"
     hPutStrLn h $ "<link>http://blog.openbrain.org/"++url++"</link>"
     hPutStrLn h $ "<description>"++para1++"</description>"
     hPutStrLn h $ "</item>"


  hPutStrLn h "</channel></rss>"