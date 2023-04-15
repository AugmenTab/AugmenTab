module Auditor.YAML
  -- FromYAML
  ( YAML.FromYAML(..)
  , YAML.withMap
  , (YAML..:)
  , (YAML..:?)

  -- ToYAML
  , YAML.ToYAML(..)
  , YAML.mapping
  , (YAML..=)

  -- Project-specific helpers
  , decodeYAML
  , decodeYAML'
  , encodeYAML
  , encodeYAML'
  ) where

import           Flipstone.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Tuple (snd)
import qualified Data.YAML as YAML

decodeYAML :: YAML.FromYAML a => LBS.ByteString -> Either T.Text a
decodeYAML = prepareErrorMessage . YAML.decode1

decodeYAML' :: YAML.FromYAML a => LBS.ByteString -> Either T.Text [a]
decodeYAML' = prepareErrorMessage . YAML.decode

prepareErrorMessage :: Either (pos, String) a -> Either T.Text a
prepareErrorMessage = mapLeft (T.pack . snd)

encodeYAML :: YAML.ToYAML a => a -> T.Text
encodeYAML = TE.decodeUtf8 . LBS.toStrict . YAML.encode1

encodeYAML' :: YAML.ToYAML a => [a] -> T.Text
encodeYAML' = TE.decodeUtf8 . LBS.toStrict . YAML.encode
