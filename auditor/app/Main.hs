module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Auditor.Config as Config
import qualified Auditor.Git as Git
import qualified Auditor.Linguist as Linguist
import           Auditor.Types

import qualified Data.Set as Set
import qualified Data.Text.IO as IO

main :: IO ()
main = do
  config  <- Config.loadConfigOrDie
  commits <- Git.getCommitHistory $ Config.auditorConfigFilepath config
  langs   <- Linguist.getLanguages config

  case langs of
    Left  err  ->
      IO.putStrLn err

    Right gitLangs -> do
      let records =
            Git.buildCommitRecord (Linguist.mkExtensionMap gitLangs) commits
              $ Set.fromList
              $ concat
                  [ authorToText <$> Config.auditorConfigAuthors config
                  , emailToText  <$> Config.auditorConfigEmails  config
                  ]

      Git.deleteCommitFile
      Git.writeCommitRecords records
      -- TODO: Build image using commit records data
      -- TODO: commit and push to GitHub when run
      IO.putStrLn "Done."
