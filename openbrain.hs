import Control.Monad
import System.Directory
import System.Cmd
import System.Exit

main = do
  --make dest, posts and static if not exist
  curdir <- getCurrentDirectory
  forM (words "dest static posts") $ createDirectoryIfMissing False

  --sync static to dest
  system "cp static/* dest/"

  --find all posts
  posts <- fmap (filter (extIn ["ibug"])) $ getDirectoryContents "posts"
  setCurrentDirectory "posts"
  forM_  posts $ \post -> do
     must $ system $ "bays --markdown "++post
     must $ system $ "pandoc -o "++setExt "html" post ++" "++setExt "md" post
     must $ system $ "cp "++setExt "html" post++" ../dest/"
  --gen html if later than in dest
  --copy to dest
  --make index from posts list
  return ()

getExt = drop 1 . dropWhile (/='.')
setExt ex = (++('.':ex)) . takeWhile (/='.')


extIn exts fnm = (getExt fnm) `elem` exts 

must :: IO ExitCode -> IO ()
must ioe = do ExitSuccess <- ioe
              return ()