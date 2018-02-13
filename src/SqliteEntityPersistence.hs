module SqliteEntityPersistence where

import           Data.String
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Entity

prepareSchema :: Connection -> IO ()
prepareSchema conn = do
  do execute_ conn $ fromString $ concat [ "CREATE TABLE EntityProperties ("
                                         , "  id INTEGER PRIMARY KEY,"
                                         , "  entityName TEXT NOT NULL,"
                                         , "  entityId INTEGER NOT NULL,"
                                         -- TODO: add constraint to check wrong propertyType
                                         , "  propertyType TEXT NOT NULL,"
                                         , "  propertyInt INTEGER,"
                                         , "  propertyText TEXT,"
                                         , "  propertyUTCTime DATETIME"
                                         , ");"
                                         ]
     execute_ conn $ fromString $ concat [ "CREATE TABLE EntityIds ("
                                         , "  entityName TEXT NOT NULL UNIQUE,"
                                         , "  entityId INTEGER NOT NULL DEFAULT 0"
                                         , ");"
                                         ]

createEntity :: T.Text -> Properties -> IO Entity
createEntity name properties = return $ Entity { entityId = 42
                                               , entityName = name
                                               , entityProperties = properties
                                               }

getEntityById :: T.Text -> Int -> IO (Maybe Entity)
getEntityById _ _ = return Nothing

getRandomEntity :: T.Text -> IO (Maybe Entity)
getRandomEntity _ = return Nothing
