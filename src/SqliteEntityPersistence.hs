{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SqliteEntityPersistence ( prepareSchema
                               , createEntity
                               , getEntityById
                               , getRandomEntity
                               , selectEntities
                               , nextEntityId
                               ) where

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Database.SQLite.Simple
import           Effect (Selector(..))
import           Entity
import           Text.RawString.QQ

data EntityIdEntry = EntityIdEntry T.Text Int

instance FromRow EntityIdEntry where
  fromRow = EntityIdEntry <$> field <*> field

nextEntityId :: Connection -> T.Text -> IO Int
nextEntityId conn name =
    do e <- queryNamed conn [r| SELECT entityName, entityId
                                FROM EntityId
                                WHERE entityName = :entityName |]
                            [ ":entityName" := name ]
       case e of
         [] -> do executeNamed conn
                               [r| INSERT INTO EntityId (
                                     entityName,
                                     entityId
                                   ) VALUES (
                                     :entityName,
                                     :entityId
                                   ) |]
                               [ ":entityName" := name
                               , ":entityId" := (1 :: Int)
                               ]
                  return 1
         [EntityIdEntry _ ident] -> do
                executeNamed conn
                             [r| UPDATE EntityId
                                 SET entityId = :entityId
                                 WHERE entityName = :entityName |]
                             [ ":entityName" := name
                             , ":entityId" := ident + 1
                             ]
                return (ident + 1)
         _ -> ioError (userError "EntityId table contains duplicate entries")


createEntityProperty :: Connection -> T.Text -> Int -> T.Text -> Property -> IO ()
createEntityProperty conn name ident propertyName property =
    executeNamed conn
                 [r| INSERT INTO EntityProperty (
                       entityName,
                       entityId,
                       propertyName,
                       propertyType,
                       propertyInt,
                       propertyText,
                       propertyUTCTime
                     ) VALUES (
                       :entityName,
                       :entityId,
                       :propertyName,
                       :propertyType,
                       :propertyInt,
                       :propertyText,
                       :propertyUTCTime
                     ) |]
                 [ ":entityName" := name
                 , ":entityId" := ident
                 , ":propertyName" := propertyName
                 , ":propertyType" := propertyTypeName property
                 , ":propertyInt" := (fromProperty property :: Maybe Int)
                 , ":propertyText" := (fromProperty property :: Maybe T.Text)
                 , ":propertyUTCTime" := (fromProperty property :: Maybe UTCTime)
                 ]

-- TODO(#53): The SQLite schema is not migrated automatically
prepareSchema :: Connection -> IO ()
prepareSchema conn =
    do
      -- TODO(#54): propertyType field of EntityProperty table of SQLiteEntityPersistence may contain incorrect values
      execute_ conn [r| CREATE TABLE IF NOT EXISTS EntityProperty (
                          id INTEGER PRIMARY KEY,
                          entityName TEXT NOT NULL,
                          entityId INTEGER NOT NULL,
                          propertyName TEXT NOT NULL,
                          propertyType TEXT NOT NULL,
                          propertyInt INTEGER,
                          propertyText TEXT,
                          propertyUTCTime DATETIME
                        ) |]
      execute_ conn [r| CREATE TABLE IF NOT EXISTS EntityId (
                          entityName TEXT NOT NULL UNIQUE,
                          entityId INTEGER NOT NULL DEFAULT 0
                        ); |]

createEntity :: Connection -> T.Text -> Properties -> IO Entity
createEntity conn name properties =
    do
      ident <- nextEntityId conn name
      mapM_ (uncurry $ createEntityProperty conn name ident) $ M.toList properties
      return Entity { entityId = ident
                    , entityName = name
                    , entityProperties = properties
                    }

getEntityById :: Connection -> T.Text -> Int -> IO (Maybe Entity)
getEntityById conn name ident =
    restoreEntity name ident
      <$> queryNamed conn [r| SELECT propertyName,
                                     propertyType,
                                     propertyInt,
                                     propertyText,
                                     propertyUTCTime
                              FROM EntityProperty
                              WHERE entityName=:entityName AND
                                    entityId=:entityId |]
                          [ ":entityName" := name
                          , ":entityId" := ident
                          ]

getRandomEntityId :: Connection -> T.Text -> Selector -> IO (Maybe Int)
getRandomEntityId conn name All =
    listToMaybe . map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY RANDOM()
                              LIMIT 1 |]
                          [ ":entityName" := name ]
getRandomEntityId conn name (PropertyEquals propertyName (PropertyText value)) =
    listToMaybe . map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                                AND propertyName = :propertyName
                                AND propertyText = :propertyText
                              GROUP BY entityId
                              ORDER BY RANDOM()
                              LIMIT 1 |]
                          [ ":entityName" := name
                          , ":propertyName" := propertyName
                          , ":propertyText" := value
                          ]
-- TODO(#138): SEP.getRandomEntityId doesn't work with Int and UTCTime on PropertyEquals
getRandomEntityId conn name (PropertyEquals _ _) =
    getRandomEntityId conn name All

getAllEntityIds :: Connection -> T.Text -> IO [Int]
getAllEntityIds conn name =
    map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY entityId |]
                          [ ":entityName" := name ]


getRandomEntity :: Connection -> T.Text -> Selector -> IO (Maybe Entity)
getRandomEntity conn name selector =
    getRandomEntityId conn name selector >>= maybe (return Nothing) (getEntityById conn name)


selectEntities :: Connection -> T.Text -> Selector -> IO [Entity]
selectEntities conn name All =
    do ids <- getAllEntityIds conn name
       fromMaybe [] . traverse id <$> traverse (getEntityById conn name) ids
selectEntities conn name (PropertyEquals propertyName propertyValue) =
    filter (\e -> M.lookup propertyName (entityProperties e) == return propertyValue)
      <$> selectEntities conn name All
