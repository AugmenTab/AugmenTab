module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Auditor.Config as Config
import qualified Auditor.Linguist as Linguist

import qualified Data.Text.IO  as IO

main :: IO ()
main = do
  config <- Config.loadConfigOrDie
  languages <- Linguist.getLanguageMap config

  IO.putStrLn $
    case languages of
      Left  err    -> err
      Right _langs -> "Languages loaded!"
