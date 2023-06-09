{-# LANGUAGE TupleSections #-}
module Auditor.Linguist
  ( getLanguages
  , mkExtensionMap
  , mkLanguageMap
  ) where

import           Flipstone.Prelude
import qualified Auditor.Config as Config
import           Auditor.Types
import           Auditor.YAML

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Encoding as TE
import           Data.Tuple (fst)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Conduit (tlsManagerSettings)

getLanguages :: Config.AuditorConfig -> IO (Either T.Text [Language])
getLanguages config = do
  IO.putStrLn "Fetching languages..."
  manager <- HTTP.newManager tlsManagerSettings
  ffmap (L.filter ((==) Programming . languageType))
    $ fmap (decodeYAML' . prepareDocument . HTTP.responseBody)
    $ HTTP.httpLbs (Config.auditorConfigRequest config) manager

mkExtensionMap :: [Language] -> Map.Map Extension LanguageName
mkExtensionMap languages =
  Map.fromList
    $ concat
    $ flip mapMaybe languages
    $ \lang -> ffmap (, languageName lang) (languageExtensions lang)

mkLanguageMap :: [Language] -> Map.Map LanguageName Language
mkLanguageMap = Map.fromList . fmap (\lang -> (languageName lang, lang))

prepareDocument :: LBS.ByteString -> LBS.ByteString
prepareDocument =
  LBS.fromStrict
    . TE.encodeUtf8
    . T.unlines
    . fmap prepareLine
    . L.drop 1
    . L.dropWhile (maybe False ((==) '#' . fst) . T.uncons)
    . T.lines
    . TE.decodeUtf8
    . LBS.toStrict

prepareLine :: T.Text -> T.Text
prepareLine txt =
  case T.uncons txt of
    Just (c, rest)
      | c /= ' ' -> "---\nname: " <> T.filter (/= ':') (T.strip $ T.cons c rest)
    _            -> T.strip txt
