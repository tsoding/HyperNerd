{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.GitHub
  ( FileName(..)
  , FileContent(..)
  , GistId(..)
  , updateGistFile
  ) where

import qualified Data.Text as T
import Effect
import Text.InterpolatedString.QM
import Data.Aeson
import Network.HTTP.Simple (getResponseStatus, parseRequest, setRequestBodyJSON)
import Network.HTTP.Types.Status (Status(..))
import Control.Monad

newtype FileName = FileName T.Text
newtype FileContent = FileContent T.Text
newtype GistId = GistId { gistIdAsText :: T.Text }

updateGistFile :: FileName -> FileContent -> GistId -> Effect ()
updateGistFile (FileName name) (FileContent content) (GistId gistId) = do
  let payload =
        object ["files" .= object [name .= object ["content" .= content]]]
  let request =
        setRequestBodyJSON payload <$>
        parseRequest [qms|PATCH https://api.github.com/gists/{gistId}|]
  response <-
    either (\e -> errorEff [qms|[ERROR] {e}|]) githubApiRequest request
  -- TODO(#634): the GitHub API error is not logged anywhere
  when (statusCode (getResponseStatus response) >= 400) $
    errorEff [qms|[ERROR] Something went wrong with GitHub API query {response}|]
