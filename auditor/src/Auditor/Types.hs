module Auditor.Types
  ( Author, authorToText
  , Email, emailToText
  , Filepath, filepathToText
  ) where

import           Auditor.YAML

import qualified Data.Text as T

{-| An Author is a textual representation of a name (either real or a username)
   to search for in the commit history.
 -}
newtype Author = Author T.Text
  deriving newtype FromYAML

authorToText :: Author -> T.Text
authorToText (Author t) = t

{-| An Email is a textual representation of an email to search for in the commit
   history.
 -}
newtype Email = Email T.Text
  deriving newtype FromYAML

emailToText :: Email -> T.Text
emailToText (Email t) = t

{-| This newtype is taking the place of `System.IO`'s `FilePath`, but instead as
   a `Text` for the purposes of dealing with `Data.YAML`.
 -}
newtype Filepath = Filepath T.Text
  deriving newtype FromYAML

filepathToText :: Filepath -> T.Text
filepathToText (Filepath t) = t
