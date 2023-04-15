module Auditor.Git
  ( getCommitHistory
  , deleteCommitFile
  ) where

import           Flipstone.Prelude
import           Auditor.Types

import           Control.Monad (filterM, mapM_)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.IO (readFile)
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import           System.Process (callCommand)

getCommitHistory :: Filepath -> IO TL.Text
getCommitHistory fp = do
  mapM_ (writeRepositoryCommits fp . T.pack)
    =<< filterM (Directory.doesDirectoryExist . (T.unpack fp </>))
    =<< Directory.listDirectory (T.unpack fp)

  readFile commitFilepathFromAuditor

commitFilepathFromRepo :: Filepath
commitFilepathFromRepo = "../commits.txt"

commitFilepathFromAuditor :: String
commitFilepathFromAuditor = "../" <> T.unpack commitFilepathFromRepo

writeRepositoryCommits :: Filepath -> Filepath -> IO ()
writeRepositoryCommits fp repo = do
  IO.putStrLn $ "Pulling and writing " <> repo <> "..."
  callCommand
    $ T.unpack
    $ T.intercalate " && "
        [ "cd " <> fp <> repo
        , "git pull"
        , "git log --numstat >> " <> commitFilepathFromRepo
        ]

deleteCommitFile :: IO ()
deleteCommitFile = do
  IO.putStrLn "Deleting commit file..."
  Directory.removeFile commitFilepathFromAuditor
