module Auditor.Git
  ( getCommitHistory
  , buildCommitRecord
  ) where

import           Flipstone.Prelude
import           Auditor.Config (AuditorConfig (..))
import           Auditor.Types

import           Control.Monad (filterM, mapM_)
import           Control.Monad.Extra (concatMapM)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.Split as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.IO (readFile)
import           Data.Tuple (uncurry)
import qualified System.Directory as Directory
import           System.Process (callCommand)
import           Text.Read (readMaybe)

getCommitHistory :: AuditorConfig -> IO TL.Text
getCommitHistory config = do
  allRepos <- concatMapM listAllRepos $ auditorConfigRepoPaths config
  excluded <- concatMapM listAllRepos $ auditorConfigExcludeRepos config

  let
    repos =
      Set.toList $
        Set.difference (Set.fromList allRepos) (Set.fromList excluded)

  mapM_ writeRepositoryCommits
    =<< filterM (Directory.doesDirectoryExist . T.unpack )
    =<< concatMapM listAllRepos repos

  readFile commitFilepathFromAuditor

listAllRepos :: Filepath -> IO [Filepath]
listAllRepos fp =
  case L.unsnoc $ T.split ('/' ==) fp of
    Just (pieces, "*") -> do
      let base = T.intercalate "/" pieces <> "/"
      ffmap ((<>) base . T.pack) . Directory.listDirectory $ T.unpack base
    _ ->
      pure [fp]

commitFilepathFromRepo :: Filepath
commitFilepathFromRepo = "../commits.txt"

commitFilepathFromAuditor :: String
commitFilepathFromAuditor = "../" <> T.unpack commitFilepathFromRepo

writeRepositoryCommits :: Filepath -> IO ()
writeRepositoryCommits repo = do
  IO.putStrLn $ "Pulling and writing " <> repo <> "..."
  callCommand
    $ T.unpack
    $ T.intercalate " && "
        [ "cd " <> repo
        , "git switch $(git remote show origin | awk '/HEAD branch/ {print $NF}')"
        , "git pull"
        , "git log --numstat >> " <> commitFilepathFromRepo
        ]

buildCommitRecord :: Map.Map Extension LanguageName
                  -> TL.Text
                  -> Set.Set T.Text
                  -> [CommitRecord]
buildCommitRecord langMap commits authorsAndEmails =
  L.foldl' mkCommitRecord []
    $ Map.assocs
    $ L.foldl' (buildCommitMap authorsAndEmails langMap) Map.empty
    $ L.splitWhen isCommitHeader
    $ TL.lines commits

mkCommitRecord :: [CommitRecord]
               -> (LanguageName, CommitData)
               -> [CommitRecord]
mkCommitRecord records (language, (insertions, deletions)) =
  (: records) $
    CommitRecord
      { commitRecordLanguage   = language
      , commitRecordInsertions = insertions
      , commitRecordDeletions  = deletions
      }

buildCommitMap :: Set.Set T.Text
               -> Map.Map Extension LanguageName
               -> Map.Map LanguageName CommitData
               -> [TL.Text]
               -> Map.Map LanguageName CommitData
buildCommitMap authorsAndEmails langMap commitMap commitLines =
  if hasDesiredAuthor authorsAndEmails commitLines
     then L.foldl' (tryParseCommitReport langMap) commitMap commitLines
     else commitMap

isCommitHeader :: TL.Text -> Bool
isCommitHeader txt =
  case TL.words txt of
    "commit" : hash : [] ->
      TL.all C.isHexDigit hash

    _ ->
      False

hasDesiredAuthor :: Set.Set T.Text -> [TL.Text] -> Bool
hasDesiredAuthor authorsAndEmails =
  L.any (L.any (`Set.member` authorsAndEmails) . T.words . TL.toStrict)

tryParseCommitReport :: Map.Map Extension LanguageName
                     -> Map.Map LanguageName CommitData
                     -> TL.Text
                     -> Map.Map LanguageName CommitData
tryParseCommitReport langMap commitMap line =
  either (const commitMap) (uncurry (insertCommitData commitMap)) $
    flip Atto.parseOnly (TL.toStrict line) $ do
      ins  <- readMaybe . T.unpack . T.strip <$> Atto.takeTill (not . C.isDigit)
      _    <- Atto.string "\t"
      dels <- readMaybe . T.unpack . T.strip <$> Atto.takeTill (not . C.isDigit)
      ext  <- T.cons '.' . T.takeWhileEnd (/= '.') . T.strip <$> Atto.takeText

      case (Map.lookup (mkExtension ext) langMap, ins, dels) of
        (Just ln, Just is, Just ds) ->
          pure (ln, (mkInsertions is, mkDeletions ds))

        _ ->
          fail "Could not parse a commit report."

insertCommitData :: Map.Map LanguageName CommitData
                 -> LanguageName
                 -> CommitData
                 -> Map.Map LanguageName CommitData
insertCommitData commitMap lang commitData =
  Map.insertWith combineCommitData lang commitData commitMap

combineCommitData :: CommitData -> CommitData -> CommitData
combineCommitData (ins1, dels1) (ins2, dels2) = (ins1 + ins2, dels1 + dels2)
