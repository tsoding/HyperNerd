{-# LANGUAGE OverloadedStrings #-}
module SqliteEntityPersistence ( prepareSchema
                               , createEntity
                               , getEntityById
                               , getRandomEntity
                               ) where

import qualified Data.Map as M
import           Data.String
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Entity

data EntityIdEntry = EntityIdEntry T.Text Int

instance FromRow EntityIdEntry where
  fromRow = EntityIdEntry <$> field <*> field

nextEntityId :: Connection -> T.Text -> IO Int
nextEntityId conn name =
    do e <- query_ conn "SELECT * from EntityId;" :: IO [EntityIdEntry]
       case e of
         [] -> do executeNamed conn
                               (fromString $ concat [ "INSERT INTO EntityId ("
                                                    , "  entityName,"
                                                    , "  entityId"
                                                    , ") VALUES ("
                                                    , "  :entityName,"
                                                    , "  :entityId"
                                                    , ")"
                                                    ])
                               [ ":entityName" := name
                               , ":entityId" := (1 :: Int)
                               ]
                  return 1
         [EntityIdEntry _ ident] -> do
                executeNamed conn
                             "UPDATE EntityId SET entityId = :entityId WHERE entityName = :entityName"
                             [ ":entityName" := name
                             , ":entityId" := ident + 1
                             ]
                return (ident + 1)
         _ -> ioError (userError "EntityId table contains duplicate entries")

createEntityProperty :: Connection -> T.Text -> Int -> T.Text -> Property -> IO ()
createEntityProperty = undefined

prepareSchema :: Connection -> IO ()
prepareSchema conn =
    withTransaction conn $ do
      execute_ conn $ fromString $ concat [ "CREATE TABLE EntityProperty ("
                                          , "  id INTEGER PRIMARY KEY,"
                                          , "  entityName TEXT NOT NULL,"
                                          , "  entityId INTEGER NOT NULL,"
                                          , "  propertyName TEXT NOT NULL,"
                                          -- TODO: add constraint to check wrong propertyType
                                          , "  propertyType TEXT NOT NULL,"
                                          , "  propertyInt INTEGER,"
                                          , "  propertyText TEXT,"
                                          , "  propertyUTCTime DATETIME"
                                          , ");"
                                          ]
      execute_ conn $ fromString $ concat [ "CREATE TABLE EntityId ("
                                          , "  entityName TEXT NOT NULL UNIQUE,"
                                          , "  entityId INTEGER NOT NULL DEFAULT 0"
                                          , ");"
                                          ]

createEntity :: Connection -> T.Text -> Properties -> IO Entity
createEntity conn name properties =
    withTransaction conn $ do
      ident <- nextEntityId conn name
      sequence_ $ map (uncurry $ createEntityProperty conn name ident) $ M.toList properties
      return $ Entity { entityId = ident
                      , entityName = name
                      , entityProperties = properties
                      }


getEntityById :: T.Text -> Int -> IO (Maybe Entity)
getEntityById _ _ = return Nothing

getRandomEntity :: T.Text -> IO (Maybe Entity)
getRandomEntity _ = return Nothing
