module Main
  ( main
  ) where

import           Flipstone.Prelude
import qualified Auditor.Config as Config

import qualified Data.Text.IO  as IO

main :: IO ()
main = do
  _config <- Config.loadConfigOrDie

  IO.putStrLn "Config loaded!"
