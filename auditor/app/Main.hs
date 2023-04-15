module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Auditor.Config as Config
import qualified Auditor.Git as Git
import qualified Auditor.Linguist as Linguist

import qualified Data.Text.IO as IO

main :: IO ()
main = do
  config  <- Config.loadConfigOrDie
  _commits <- Git.getCommitHistory $ Config.auditorConfigFilepath config
  _langMap <- ffmap Linguist.mkLanguageMap $ Linguist.getLanguages config

  Git.deleteCommitFile
  IO.putStrLn "Done."
