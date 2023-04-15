module Auditor.YAML
  ( YAML.FromYAML(..)
  , (YAML..:)
  , (YAML..:?)
  , YAML.withMap

  -- Project-specific helpers
  , decodeYAML
  , decodeYAML'
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Tuple (snd)
import qualified Data.YAML as YAML

decodeYAML :: YAML.FromYAML a => LBS.ByteString -> Either T.Text a
decodeYAML = prepareErrorMessage . YAML.decode1

decodeYAML' :: YAML.FromYAML a => LBS.ByteString -> Either T.Text [a]
decodeYAML' = prepareErrorMessage . YAML.decode

prepareErrorMessage :: Either (pos, String) a -> Either T.Text a
prepareErrorMessage = mapLeft (T.pack . snd)
