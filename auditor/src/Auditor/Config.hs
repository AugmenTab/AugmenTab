module Auditor.Config
  ( AuditorConfig(..)
  , loadConfigOrDie
  ) where

import           Flipstone.Prelude
import           Auditor.Types
import           Auditor.YAML

import           Control.Exception (SomeException, catch)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Exit (ExitCode(..), exitWith)
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP
import qualified System.IO as IO
import           Text.Show (show)

data AuditorConfig =
  AuditorConfig
    { auditorConfigAuthors      :: [Author]
    , auditorConfigEmails       :: [Email]
    , auditorConfigRepoPaths    :: [Filepath]
    , auditorConfigExcludeRepos :: [Filepath]
    , auditorConfigRequest      :: HTTP.Request
    }

instance FromYAML AuditorConfig where
  parseYAML = withMap "AuditorConfig" $ \m ->
    AuditorConfig
      <$> m .: "authors"
      <*> m .: "emails"
      <*> m .: "repo_paths"
      <*> m .: "exclude_repos"
      <*> (parseURL =<< m .: "languages_url")

parseURL :: MonadFail m => T.Text -> m HTTP.Request
parseURL =
  maybe (fail "URL is not a valid format") pure . HTTP.parseRequest . T.unpack

loadConfigOrDie :: IO AuditorConfig
loadConfigOrDie = do
  let path = "./config.yaml"

  IO.putStrLn $ "Loading config from path: " <> path
  configBytes <- LBS8.readFile path `catch` exitException 66

  let errLabel =
           "Error parsing Auditor config\n\n"
        <> LBS8.unpack configBytes
        <> "\n\nError was"

  either (exitErrorLabel 66 errLabel . T.unpack) pure $ decodeYAML configBytes

exitException :: Int -> SomeException -> IO a
exitException code =
  exitErrorLabel code "Error loading Auditor config" . show

exitErrorLabel :: Int -> String -> String -> IO a
exitErrorLabel code label message =
  exitError code $ label <> ":\n" <> message

exitError :: Int -> String -> IO a
exitError code message = do
  IO.hPutStrLn IO.stderr message
  exitWith $ ExitFailure code
