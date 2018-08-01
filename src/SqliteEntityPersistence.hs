{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SqliteEntityPersistence ( prepareSchema
                               , createEntity
                               , getEntityById
                               , selectEntities
                               , deleteEntities
                               , updateEntities
                               , nextEntityId
                               ) where

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Database.SQLite.Simple
import           Effect (Selector(..), Condition(..), Action(..))
import           Entity
import           Property
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

deleteEntityById :: Connection -> T.Text -> Int -> IO ()
deleteEntityById conn name ident =
    executeNamed conn [r| DELETE
                          FROM EntityProperty
                          WHERE entityName=:entityName AND
                                entityId=:entityId |]
                      [ ":entityName" := name
                      , ":entityId" := ident
                      ]

selectEntities :: Connection -> T.Text -> Selector -> IO [Entity]
selectEntities conn name selector =
    do ids <- selectEntityIds conn name selector
       fromMaybe [] . traverse id <$> traverse (getEntityById conn name) ids

selectEntityIds :: Connection -> T.Text -> Selector -> IO [Int]
selectEntityIds conn name All =
    map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY entityId |]
                          [ ":entityName" := name ]
selectEntityIds conn name (Filter (PropertyEquals propertyName property) All) =
    map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                                AND propertyName = :propertyName
                                AND propertyInt IS :propertyIntValue
                                AND propertyText IS :propertyTextValue
                                AND propertyUTCTime IS :propertyUTCTime
                              GROUP BY entityId |]
                          [ ":entityName" := name
                          , ":propertyName" := propertyName
                          , ":propertyIntValue" := (fromProperty property :: Maybe Int)
                          , ":propertyTextValue" := (fromProperty property :: Maybe T.Text)
                          , ":propertyUTCTime" := (fromProperty property :: Maybe UTCTime)
                          ]
selectEntityIds conn name (Shuffle All) =
    map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY RANDOM() |]
                          [ ":entityName" := name ]
selectEntityIds conn name (Take n (Shuffle All)) =
    map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY RANDOM()
                              LIMIT :n |]
                          [ ":entityName" := name
                          , ":n" := n
                          ]
selectEntityIds conn name (Take n (Shuffle (Filter (PropertyEquals propertyName property) All))) =
    map fromOnly
      <$> queryNamed conn [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                                AND propertyName = :propertyName
                                AND propertyInt IS :propertyIntValue
                                AND propertyText IS :propertyTextValue
                                AND propertyUTCTime IS :propertyUTCTime
                              GROUP BY entityId
                              ORDER BY RANDOM()
                              LIMIT :n |]
                          [ ":entityName" := name
                          , ":propertyName" := propertyName
                          , ":propertyIntValue" := (fromProperty property :: Maybe Int)
                          , ":propertyTextValue" := (fromProperty property :: Maybe T.Text)
                          , ":propertyUTCTime" := (fromProperty property :: Maybe UTCTime)
                          , ":n" := n
                          ]
-- TODO(#178): SEP.selectEntityIds doesn't support arbitrary selector combination
selectEntityIds _ _ selector =
    error ("Unsupported selector combination " ++ show selector)

deleteEntities :: Connection    -- conn
               -> T.Text        -- name
               -> Selector      -- selector
               -> IO Int
deleteEntities conn name selector =
    do ids <- selectEntityIds conn name selector
       length <$> traverse (deleteEntityById conn name) ids

-- TODO: SEP.updateEntities is not implemented
updateEntities :: Connection    -- conn
               -> T.Text        -- name
               -> Selector      -- selector
               -> Action        -- action
               -> IO Int
updateEntities _ _ _ _ = return 0
