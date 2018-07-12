{-# LANGUAGE OverloadedStrings #-}
module Bot.Periodic ( addPeriodicMessage
                    , startPeriodicMessages
                    ) where

import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Effect
import           Entity

data PeriodicMessage = PeriodicMessage { pmText :: T.Text
                                       , pmAuthor :: T.Text
                                       , pmCreatedAt :: UTCTime
                                       }

instance IsEntity PeriodicMessage where
    toProperties pm =
        M.fromList [ ("text", PropertyText $ pmText pm)
                   , ("author", PropertyText $ pmAuthor pm)
                   , ("createdAt", PropertyUTCTime $ pmCreatedAt pm)
                   ]
    fromEntity e = do text      <- extractProperty "text" e
                      author    <- extractProperty "author" e
                      createdAt <- extractProperty "createdAt" e
                      return PeriodicMessage { pmText = text
                                             , pmAuthor = author
                                             , pmCreatedAt = createdAt
                                             }

addPeriodicMessage :: T.Text -> Effect ()
addPeriodicMessage _ = undefined

startPeriodicMessages :: Effect ()
startPeriodicMessages = undefined
