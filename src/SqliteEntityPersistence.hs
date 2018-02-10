module SqliteEntityPersistence where

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Entity

-- TODO(#49): Implement SqliteEntityPersistance module

prepareSchema :: Connection -> IO ()
prepareSchema _ = return ()

saveEntity :: T.Text -> Properties -> IO Entity
saveEntity name properties = return $ Entity { entityId = 42
                                             , entityName = name
                                             , entityProperties = properties
                                             }

getEntityById :: T.Text -> Int -> IO (Maybe Entity)
getEntityById _ _ = return Nothing

getRandomEntity :: T.Text -> IO (Maybe Entity)
getRandomEntity _ = return Nothing
