{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sqlite.EntityPersistence ( prepareSchema
                                , createEntity
                                , getEntityById
                                , updateEntityById
                                , selectEntities
                                , deleteEntities
                                , updateEntities
                                , deleteEntityById
                                , nextEntityId
                                , entityNames
                                ) where

import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Database.SQLite.Simple
import           Effect (Selector(..), Condition(..), Order(..))
import           Entity
import           Property
import           Sqlite.Migration
import           Text.InterpolatedString.QM

data EntityIdEntry = EntityIdEntry T.Text Int

instance FromRow EntityIdEntry where
  fromRow = EntityIdEntry <$> field <*> field

entityNames :: Connection -> IO [T.Text]
entityNames conn =
    map fromOnly
      <$> query_ conn [qms| SELECT DISTINCT entityName
                          FROM EntityProperty
                          GROUP BY entityName |]

nextEntityId :: Connection -> T.Text -> IO Int
nextEntityId conn name =
    do e <- queryNamed conn [qms| SELECT entityName, entityId
                                FROM EntityId
                                WHERE entityName = :entityName |]
                            [ ":entityName" := name ]
       case e of
         [] -> do executeNamed conn
                               [qms| INSERT INTO EntityId (
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
                             [qms| UPDATE EntityId
                                 SET entityId = :entityId
                                 WHERE entityName = :entityName |]
                             [ ":entityName" := name
                             , ":entityId" := ident + 1
                             ]
                return (ident + 1)
         _ -> ioError (userError "EntityId table contains duplicate entries")


createEntityProperty :: Connection
                     -> T.Text
                     -> Int
                     -> T.Text
                     -> Property
                     -> IO ()
createEntityProperty conn name ident propertyName property =
    executeNamed conn
                 [qms| INSERT INTO EntityProperty (
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

-- TODO(#54): propertyType field of EntityProperty table of SQLiteEntityPersistence may contain incorrect values
entityMigrations :: [Migration]
entityMigrations =
    [ [qms| CREATE TABLE IF NOT EXISTS EntityProperty (
            id INTEGER PRIMARY KEY,
            entityName TEXT NOT NULL,
            entityId INTEGER NOT NULL,
            propertyName TEXT NOT NULL,
            propertyType TEXT NOT NULL,
            propertyInt INTEGER,
            propertyText TEXT,
            propertyUTCTime DATETIME
          ) |]
    , [qms| CREATE TABLE IF NOT EXISTS EntityId (
            entityName TEXT NOT NULL UNIQUE,
            entityId INTEGER NOT NULL DEFAULT 0
          ); |]
    , [qms| CREATE TABLE IF NOT EXISTS EntityProperty_Unique (
            id INTEGER PRIMARY KEY,
            entityName TEXT NOT NULL,
            entityId INTEGER NOT NULL,
            propertyName TEXT NOT NULL,
            propertyType TEXT NOT NULL,
            propertyInt INTEGER,
            propertyText TEXT,
            propertyUTCTime DATETIME,
            UNIQUE(entityName, entityId, propertyName) ON CONFLICT REPLACE
          ); |]
    , [qms| INSERT INTO EntityProperty_Unique
          SELECT * FROM EntityProperty; |]
    , [qms| DROP TABLE EntityProperty; |]
    , [qms| ALTER TABLE EntityProperty_Unique
          RENAME TO EntityProperty; |]
    ]

prepareSchema :: Connection -> IO ()
prepareSchema conn = migrateDatabase conn entityMigrations

createEntity :: Connection -> T.Text -> Properties -> IO (Entity Properties)
createEntity conn name properties =
    do
      ident <- nextEntityId conn name
      mapM_ (uncurry $ createEntityProperty conn name ident) $ M.toList properties
      return Entity { entityId = ident
                    , entityName = name
                    , entityPayload = properties
                    }

getEntityById :: Connection -> T.Text -> Int -> IO (Maybe (Entity Properties))
getEntityById conn name ident =
    restoreEntity name ident
      <$> queryNamed conn [qms| SELECT propertyName,
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
    executeNamed conn [qms| DELETE
                          FROM EntityProperty
                          WHERE entityName=:entityName AND
                                entityId=:entityId |]
                      [ ":entityName" := name
                      , ":entityId" := ident
                      ]

-- TODO(#251): reimplement selectEntities with Sqlite.Compiler.compileSelector when it's done
selectEntities :: Connection -> T.Text -> Selector -> IO [Entity Properties]
selectEntities conn name selector =
    do ids <- selectEntityIds conn name selector
       fromMaybe [] . traverse id <$> traverse (getEntityById conn name) ids

selectEntityIds :: Connection -> T.Text -> Selector -> IO [Int]
selectEntityIds conn name All =
    map fromOnly
      <$> queryNamed conn [qms| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY entityId |]
                          [ ":entityName" := name ]
selectEntityIds conn name (Filter (PropertyEquals propertyName property) All) =
    map fromOnly
      <$> queryNamed conn [qms| SELECT entityId
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
selectEntityIds conn name (Take n (Filter (PropertyEquals propertyName property) All)) =
    map fromOnly
      <$> queryNamed conn [qms| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                                AND propertyName = :propertyName
                                AND propertyInt IS :propertyIntValue
                                AND propertyText IS :propertyTextValue
                                AND propertyUTCTime IS :propertyUTCTime
                              GROUP BY entityId
                              LIMIT :n|]
                          [ ":entityName" := name
                          , ":propertyName" := propertyName
                          , ":propertyIntValue" := (fromProperty property :: Maybe Int)
                          , ":propertyTextValue" := (fromProperty property :: Maybe T.Text)
                          , ":propertyUTCTime" := (fromProperty property :: Maybe UTCTime)
                          , ":n" := n
                          ]
selectEntityIds conn name (Shuffle All) =
    map fromOnly
      <$> queryNamed conn [qms| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY RANDOM() |]
                          [ ":entityName" := name ]
selectEntityIds conn name (Take n (Shuffle All)) =
    map fromOnly
      <$> queryNamed conn [qms| SELECT entityId
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
      <$> queryNamed conn [qms| SELECT entityId
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
-- TODO(#255): SortBy selector supports only UTCTime properties
-- TODO(#256): SortBy selector supports only Desc order
selectEntityIds conn name (Take n (SortBy propertyName Desc All)) =
    map fromOnly
      <$> queryNamed conn [qms| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                                AND propertyName is :propertyName
                              GROUP BY entityId
                              ORDER BY propertyUTCTime DESC
                              LIMIT :n |]
                          [ ":entityName" := name
                          , ":propertyName" := propertyName
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

-- TODO(#184): SEP.updateEntities is not implemented
updateEntities :: Connection    -- conn
               -> T.Text        -- name
               -> Selector      -- selector
               -> Properties    -- properties
               -> IO Int
updateEntities _ _ _ _ = return 0

{-# ANN updateEntityById ("HLint: ignore Use fmap" :: String) #-}
{-# ANN updateEntityById ("HLint: ignore Use <$>" :: String) #-}
updateEntityById :: Connection        -- conn
                 -> Entity Properties -- entity
                 -> IO (Maybe (Entity Properties))
updateEntityById conn entity =
    do oldEntity <- getEntityById conn (entityName entity) (entityId entity)
       maybe (return Nothing)
             (\_ -> do traverse_ (uncurry (createEntityProperty conn name ident))
                         $ M.toList
                         $ entityPayload entity
                       return $ Just entity)
             oldEntity
    where name = entityName entity
          ident = entityId entity
