module Auditor.Types
  ( Author, authorToText
  , Color
  , CommitData
  , CommitRecord(..)
  , Deletions, mkDeletions
  , Email, emailToText
  , Extension, mkExtension
  , Filepath
  , Insertions, mkInsertions
  , Language(..)
  , LanguageName
  , LanguageType(..), languageTypeFromText
  ) where

import           Flipstone.Prelude
import           Auditor.YAML

import qualified Data.Text as T
import           Numeric.Natural (Natural)

{-| An Author is a textual representation of a name (either real or a username)
   to search for in the commit history.
 -}
newtype Author = Author T.Text
  deriving newtype FromYAML

authorToText :: Author -> T.Text
authorToText (Author t) = t

{-| A Color is a textual representation of the hex color code assigned to a
   programming language in the linguist docs.
 -}
newtype Color = Color T.Text
  deriving newtype FromYAML

{-| CommitData is a pair of `Insertions` and `Deletions` for a given language -
   it appears in a `Map` and is looked up by the associated `LanguageName`.
 -}
type CommitData = (Insertions, Deletions)

{-| A CommitRecord is a basic representation of the summed insertions and
   deletions for a given language across all commits in all repos polled.
 -}
data CommitRecord =
  CommitRecord
    { commitRecordLanguage   :: LanguageName
    , commitRecordInsertions :: Insertions
    , commitRecordDeletions  :: Deletions
    }

instance ToYAML CommitRecord where
  toYAML cr =
    mapping [ "language"   .= commitRecordLanguage   cr
            , "insertions" .= commitRecordInsertions cr
            , "deletions"  .= commitRecordDeletions  cr
            ]

{-| Deletions are a positive number representing all lines of deleted code for a
   given language.
 -}
newtype Deletions = Deletions Natural
  deriving newtype (Num, ToYAML)

mkDeletions :: Natural -> Deletions
mkDeletions = Deletions

{-| An Email is a textual representation of an email to search for in the commit
   history.
 -}
newtype Email = Email T.Text
  deriving newtype FromYAML

emailToText :: Email -> T.Text
emailToText (Email t) = t

{-| An Extension is a text representation of a file extension for a programming
   language.
 -}
newtype Extension = Extension T.Text
  deriving newtype (Eq, Ord, FromYAML)

mkExtension :: T.Text -> Extension
mkExtension = Extension

{-| Filepath is taking the place of `System.IO`'s `FilePath`, but instead as a
   `Text` for the purposes of dealing with `Data.YAML`.
 -}
type Filepath = T.Text

{-| Insertions are a positive number representing all lines of newly-written or
   otherwise modified code for a given language.
 -}
newtype Insertions = Insertions Natural
  deriving newtype (Num, ToYAML)

mkInsertions :: Natural -> Insertions
mkInsertions = Insertions

{-| A Language is a record with relevant information for a programming language
   as it is represented in linguist.
 -}
data Language =
  Language
    { languageName       :: LanguageName
    , languageType       :: LanguageType
    , languageColor      :: Maybe Color
    , languageExtensions :: Maybe [Extension]
    }

instance FromYAML Language where
  parseYAML = withMap "Language" $ \m ->
    Language
      <$> m .:  "name"
      <*> (tryParseLanguageType =<< m .: "type")
      <*> m .:? "color"
      <*> m .:? "extensions"

{-| This is the textual representation of a language - in the linguist language
   list, this is the key for each part.
 -}
newtype LanguageName = LanguageName T.Text
  deriving newtype (Eq, FromYAML, Ord, ToYAML)

data LanguageType
  = Data
  | Markup
  | Programming
  | Prose
  | Nil

languageTypeFromText :: T.Text -> Either T.Text LanguageType
languageTypeFromText txt =
  case txt of
    "data"        -> Right Data
    "markup"      -> Right Markup
    "programming" -> Right Programming
    "prose"       -> Right Prose
    "nil"         -> Right Nil
    _ -> Left $ "Unrecognized LanguageType " <> txt

tryParseLanguageType :: MonadFail m => T.Text -> m LanguageType
tryParseLanguageType = either (fail . T.unpack) pure . languageTypeFromText
