{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Alias ( redirectAlias
                 , addAliasCommand
                 , removeAliasCommand
                 ) where

import           Bot.Replies
import           Command
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Property
import           Text.InterpolatedString.QM

data Alias = Alias { aliasName :: T.Text
                   , aliasRedirect :: T.Text
                   }

instance IsEntity Alias where
    toProperties alias =
        M.fromList [ ("name", PropertyText $ aliasName alias)
                   , ("redirect", PropertyText $ aliasRedirect alias)
                   ]
    fromProperties properties =
        Alias <$> extractProperty "name" properties
              <*> extractProperty "redirect" properties

getAliasByName :: T.Text -> Effect (Maybe Alias)
getAliasByName name =
  fmap entityPayload . (>>= fromEntityProperties) . listToMaybe
    <$> selectEntities "Alias" (Take 1 $ Filter (PropertyEquals "name" (PropertyText name)) All)

redirectAlias :: Command a -> Effect (Command a)
redirectAlias command =
    do alias <- getAliasByName $ commandName command
       return $ maybe command
                      (renameCommand command . aliasRedirect)
                      alias

addAliasCommand :: CommandHandler (T.Text, T.Text)
addAliasCommand sender (name, redirect)
    | name == redirect = replyToSender sender "Alias cannot redirect to itself"
    | otherwise = do alias <- getAliasByName name
                     case alias of
                       Just _ -> replyToSender sender [qms|Alias '{name}' already exists|]
                       Nothing -> do _ <- createEntity "Alias" Alias { aliasName = name
                                                                     , aliasRedirect = redirect
                                                                     }
                                     replyToSender sender [qms|Alias '{name}' has been created|]

removeAliasCommand :: CommandHandler T.Text
removeAliasCommand sender name = do
  alias <- getAliasByName name
  case alias of
    Just _ -> do
      _ <- deleteEntities "Alias" (Filter (PropertyEquals "name" (PropertyText name)) All)
      replyToSender sender [qms|Alias '{name}' has been removed|]
    Nothing -> replyToSender sender [qms|Alias '{name}' does not exists"|]
