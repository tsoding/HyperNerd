{-# LANGUAGE OverloadedStrings #-}
module Bot.Alias ( redirectAlias
                 , addAliasCommand
                 , removeAliasCommand
                 ) where

import           Command
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Effect
import           Entity
import           Property

data Alias = Alias { aliasOrigin :: T.Text
                   , aliasRedirect :: T.Text
                   }

instance IsEntity Alias where
    toProperties alias =
        M.fromList [ ("origin", PropertyText $ aliasOrigin alias)
                   , ("redirect", PropertyText $ aliasOrigin alias)
                   ]
    fromProperties entity =
        do origin   <- extractProperty "origin" entity
           redirect <- extractProperty "redirect" entity
           alias    <- return Alias { aliasOrigin = origin
                                    , aliasRedirect = redirect
                                    }
           return (const alias <$> entity)

getAliasByOrigin :: T.Text -> Effect (Maybe Alias)
getAliasByOrigin origin =
  fmap entityPayload . (>>= fromProperties) . listToMaybe
    <$> selectEntities "Alias" (Take 1 $ Filter (PropertyEquals "origin" (PropertyText origin)) $ All)

redirectAlias :: Command a -> Effect (Command a)
redirectAlias command =
    do alias <- getAliasByOrigin $ commandName command
       return $ maybe command
                (renameCommand command . aliasRedirect)
                alias

addAliasCommand :: CommandHandler T.Text
addAliasCommand _ _ = say "Not implemented HyperNyard"

removeAliasCommand :: CommandHandler T.Text
removeAliasCommand _ _ = say "Not implemented HyperNyard"
