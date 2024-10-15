module Main
  ( main
  , test
  ) where

import           Flipstone.Prelude
import qualified Auditor.Config as Config
import qualified Auditor.Git as Git
import qualified Auditor.Linguist as Linguist
import qualified Auditor.SVG as SVG
import           Auditor.Types
import           Auditor.YAML

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as IO

main :: IO ()
main = do
  config  <- Config.loadConfigOrDie
  commits <- Git.getCommitHistory config
  langs   <- Linguist.getLanguages config

  case langs of
    Left  err  ->
      IO.putStrLn err

    Right gitLangs -> do
      IO.putStrLn "Building SVG..."

      let
        commitRecord =
          Git.buildCommitRecord (Linguist.mkExtensionMap gitLangs) commits
            $ Set.fromList
            $ concat
                [ authorToText <$> Config.auditorConfigAuthors config
                , emailToText  <$> Config.auditorConfigEmails  config
                ]

      IO.writeFile "../commit-records.txt" $ encodeYAML commitRecord
      SVG.makeProfileSVG (Linguist.mkLanguageMap gitLangs) commitRecord

      -- TODO: commit and push to GitHub when run
      IO.putStrLn "Done."

test :: IO ()
test = SVG.makeProfileSVG Map.empty []
