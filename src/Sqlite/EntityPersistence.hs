{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Sqlite.EntityPersistence
  ( prepareSchema
  , createEntity
  , getEntityById
  , updateEntityById
  , selectEntities
  , deleteEntities
  , deleteEntityById
  , nextEntityId
  , entityNames
  ) where

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Effect (Condition(..), Order(..), Selector(..))
import Entity
import Property
import Sqlite.Migration
import Text.RawString.QQ

data EntityIdEntry =
  EntityIdEntry T.Text
                Int

instance FromRow EntityIdEntry where
  fromRow = EntityIdEntry <$> field <*> field

entityNames :: Connection -> IO [T.Text]
entityNames conn =
  map fromOnly <$>
  query_
    conn
    [r| SELECT DISTINCT entityName
                          FROM EntityProperty
                          GROUP BY entityName |]

nextEntityId :: Connection -> T.Text -> IO Int
nextEntityId conn name = do
  e <-
    queryNamed
      conn
      [r| SELECT entityName, entityId
                                FROM EntityId
                                WHERE entityName = :entityName |]
      [":entityName" := name]
  case e of
    [] -> do
      executeNamed
        conn
        [r| INSERT INTO EntityId (
                                     entityName,
                                     entityId
                                   ) VALUES (
                                     :entityName,
                                     :entityId
                                   ) |]
        [":entityName" := name, ":entityId" := (1 :: Int)]
      return 1
    [EntityIdEntry _ ident] -> do
      executeNamed
        conn
        [r| UPDATE EntityId
                                 SET entityId = :entityId
                                 WHERE entityName = :entityName |]
        [":entityName" := name, ":entityId" := ident + 1]
      return (ident + 1)
    _ -> ioError (userError "EntityId table contains duplicate entries")

createEntityProperty ::
     Connection -> T.Text -> Int -> T.Text -> Property -> IO ()
createEntityProperty conn name ident propertyName property =
  executeNamed
    conn
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

-- TODO(#54): propertyType field of EntityProperty table of SQLiteEntityPersistence may contain incorrect values
entityMigrations :: [Migration]
entityMigrations =
  [ [r| CREATE TABLE IF NOT EXISTS EntityProperty (
            id INTEGER PRIMARY KEY,
            entityName TEXT NOT NULL,
            entityId INTEGER NOT NULL,
            propertyName TEXT NOT NULL,
            propertyType TEXT NOT NULL,
            propertyInt INTEGER,
            propertyText TEXT,
            propertyUTCTime DATETIME
          ) |]
  , [r| CREATE TABLE IF NOT EXISTS EntityId (
            entityName TEXT NOT NULL UNIQUE,
            entityId INTEGER NOT NULL DEFAULT 0
          ); |]
  , [r| CREATE TABLE IF NOT EXISTS EntityProperty_Unique (
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
  , [r| INSERT INTO EntityProperty_Unique
          SELECT * FROM EntityProperty; |]
  , [r| DROP TABLE EntityProperty; |]
  , [r| ALTER TABLE EntityProperty_Unique
          RENAME TO EntityProperty; |]
  , [r| UPDATE EntityProperty
        SET propertyText = 'TwitchChannel "' || propertyText || '"'
        WHERE entityName = 'LogRecord'
          AND propertyName = 'channel'
          AND propertyText LIKE '#%';|]
  , [r| UPDATE EntityProperty
        SET propertyText = 'DiscordChannel ' || propertyText
        WHERE entityName = 'LogRecord'
          AND propertyName = 'channel'
          AND propertyText NOT LIKE 'TwitchChannel%'
          AND propertyText NOT LIKE 'DiscordChannel%'
          AND propertyText NOT LIKE '#%';|]
  , [r| INSERT INTO EntityProperty (
            entityName,
            entityId,
            propertyName,
            propertyType,
            propertyInt
       )
       SELECT entityName,
              entityId,
              'timer' AS propertyName,
              'PropertyInt' AS propertyType,
              1 AS propertyInt
       FROM EntityProperty
       WHERE entityName = 'PeriodicCommand'
       GROUP BY entityId; |]
  , [r| INSERT INTO EntityProperty (
            entityName,
            entityId,
            propertyName,
            propertyType,
            propertyInt
       )
       SELECT entityName,
              entityId,
              'period' AS propertyName,
              'PropertyInt' AS propertyType,
              (10 * 60 * 1000) AS propertyInt
       FROM EntityProperty
       WHERE entityName = 'PeriodicTimer'
       GROUP BY entityId;|]
  ]

prepareSchema :: Connection -> IO ()
prepareSchema conn = migrateDatabase conn entityMigrations

createEntity :: Connection -> T.Text -> Properties -> IO (Entity Properties)
createEntity conn name properties = do
  ident <- nextEntityId conn name
  mapM_ (uncurry $ createEntityProperty conn name ident) $ M.toList properties
  return
    Entity {entityId = ident, entityName = name, entityPayload = properties}

getEntityById :: Connection -> T.Text -> Int -> IO (Maybe (Entity Properties))
getEntityById conn name ident =
  restoreEntity name ident <$>
  queryNamed
    conn
    [r| SELECT propertyName,
                                     propertyType,
                                     propertyInt,
                                     propertyText,
                                     propertyUTCTime
                              FROM EntityProperty
                              WHERE entityName=:entityName AND
                                    entityId=:entityId |]
    [":entityName" := name, ":entityId" := ident]

deleteEntityById :: Connection -> T.Text -> Int -> IO ()
deleteEntityById conn name ident =
  executeNamed
    conn
    [r| DELETE
                          FROM EntityProperty
                          WHERE entityName=:entityName AND
                                entityId=:entityId |]
    [":entityName" := name, ":entityId" := ident]

-- TODO(#251): reimplement selectEntities with Sqlite.Compiler.compileSelector when it's done
selectEntities :: Connection -> T.Text -> Selector -> IO [Entity Properties]
selectEntities conn name selector = do
  ids <- selectEntityIds conn name selector
  fromMaybe [] . traverse id <$> traverse (getEntityById conn name) ids

selectEntityIds :: Connection -> T.Text -> Selector -> IO [Int]
selectEntityIds conn name All =
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY entityId |]
    [":entityName" := name]
selectEntityIds conn name (Filter (PropertyEquals propertyName property) All) =
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
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
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
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
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY RANDOM() |]
    [":entityName" := name]
selectEntityIds conn name (Take n (Shuffle All)) =
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                              GROUP BY entityId
                              ORDER BY RANDOM()
                              LIMIT :n |]
    [":entityName" := name, ":n" := n]
selectEntityIds conn name (Take n (Shuffle (Filter (PropertyEquals propertyName property) All))) =
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
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
selectEntityIds conn name (Take n (Shuffle (Filter (ConditionAnd [PropertyEquals propertyName1 property1, PropertyEquals propertyName2 property2]) All))) =
  map fromOnly <$>
  queryNamed
    conn
    [r| select eid from (select entityId eid, count(entityId) ceid
        from EntityProperty
        where entityName = :entityName
          and ((propertyName = :propertyName1 and
                propertyInt is :propertyIntValue1 and
                propertyText is :propertyTextValue1 and
                propertyUTCTime is :propertyUTCTimeValue1)
               or
               (propertyName = :propertyName2 and
                propertyInt is :propertyIntValue2 and
                propertyText is :propertyTextValue2 and
                propertyUTCTime is :propertyUTCTimeValue2))
        group by entityId)
        where ceid = 2
        ORDER BY RANDOM()
        LIMIT :n; |]
    [ ":entityName" := name
    , ":propertyName1" := propertyName1
    , ":propertyIntValue1" := (fromProperty property1 :: Maybe Int)
    , ":propertyTextValue1" := (fromProperty property1 :: Maybe T.Text)
    , ":propertyUTCTimeValue1" := (fromProperty property1 :: Maybe UTCTime)
    , ":propertyName2" := propertyName2
    , ":propertyIntValue2" := (fromProperty property2 :: Maybe Int)
    , ":propertyTextValue2" := (fromProperty property2 :: Maybe T.Text)
    , ":propertyUTCTimeValue2" := (fromProperty property2 :: Maybe UTCTime)
    , ":n" := n
    ]
-- TODO(#255): SortBy selector supports only UTCTime properties
-- TODO(#256): SortBy selector supports only Desc order
selectEntityIds conn name (Take n (SortBy propertyName Desc All)) =
  map fromOnly <$>
  queryNamed
    conn
    [r| SELECT entityId
                              FROM EntityProperty
                              WHERE entityName = :entityName
                                AND propertyName is :propertyName
                              GROUP BY entityId
                              ORDER BY propertyUTCTime DESC
                              LIMIT :n |]
    [":entityName" := name, ":propertyName" := propertyName, ":n" := n]
selectEntityIds conn name (SortBy propertyName1 Asc (Filter (PropertyGreater propertyName2 (PropertyUTCTime propertyUTCTime)) All))
  | propertyName1 == propertyName2 =
    map fromOnly <$>
    queryNamed
      conn
      [r| SELECT entityId
        FROM EntityProperty
        WHERE entityName = :entityName
          AND propertyName = :propertyName
          AND propertyUTCTime > :propertyUTCTime
          GROUP BY entityId
          ORDER BY propertyUTCTime ASC; |]
      [ ":entityName" := name
      , ":propertyName" := propertyName
      , ":propertyUTCTime" := propertyUTCTime
      ]
  where
    propertyName = propertyName1
selectEntityIds _ _ selector =
  error ("Unsupported selector combination " ++ show selector)

deleteEntities ::
     Connection -- conn
  -> T.Text -- name
  -> Selector -- selector
  -> IO Int
deleteEntities conn name selector = do
  ids <- selectEntityIds conn name selector
  length <$> traverse (deleteEntityById conn name) ids

updateEntityById ::
     Connection -- conn
  -> Entity Properties -- entity
  -> IO (Maybe (Entity Properties))
updateEntityById conn entity = do
  oldEntity <- getEntityById conn (entityName entity) (entityId entity)
  maybe
    (return Nothing)
    (\_ -> do
       traverse_ (uncurry (createEntityProperty conn name ident)) $
         M.toList $ entityPayload entity
       return $ Just entity)
    oldEntity
  where
    name = entityName entity
    ident = entityId entity
