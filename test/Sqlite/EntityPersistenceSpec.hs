{-# LANGUAGE OverloadedStrings #-}

module Sqlite.EntityPersistenceSpec where

import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Database.SQLite.Simple as SQLite
import Effect (Condition(..), Selector(..))
import Entity
import Property
import qualified Sqlite.EntityPersistence as SEP
import System.IO.Temp
import Test.HUnit

withTempSqliteDatabase :: (SQLite.Connection -> IO a) -> IO a
withTempSqliteDatabase action = do
  databaseFile <- emptySystemTempFile "database"
  SQLite.withConnection databaseFile action

doublePrepareSchemaSpec :: Test
doublePrepareSchemaSpec =
  TestLabel "Double Prepare SEP Schema" $
  TestCase $ do
    databaseFile <- emptySystemTempFile "database"
    SQLite.withConnection databaseFile SEP.prepareSchema
    SQLite.withConnection databaseFile SEP.prepareSchema

createEntityAndGetItById :: Test
createEntityAndGetItById =
  TestLabel "Create an Entity and get it by id" $
  TestCase $ do
    databaseFile <- emptySystemTempFile "database"
    SQLite.withConnection databaseFile $ \conn -> do
      SEP.prepareSchema conn
      storedEntity <-
        SEP.createEntity conn "entity" $
        M.fromList [("foo", PropertyInt 42), ("bar", PropertyText "hello")]
      restoredEntity <- SEP.getEntityById conn "entity" $ entityId storedEntity
      assertEqual
        "Unexpected restored entity"
        (Just storedEntity)
        restoredEntity

createSeveralEntityTypes :: Test
createSeveralEntityTypes =
  TestLabel "Create several Entity types" $
  TestCase $ do
    databaseFile <- emptySystemTempFile "database"
    SQLite.withConnection databaseFile $ \conn -> do
      SEP.prepareSchema conn
      void $
        SEP.createEntity conn "entity1" $
        M.fromList [("foo", PropertyInt 42), ("bar", PropertyText "hello")]
      void $
        SEP.createEntity conn "entity2" $
        M.fromList [("baz", PropertyInt 43), ("kamaz", PropertyText "world")]
      void $
        SEP.createEntity conn "entity2" $
        M.fromList [("baz", PropertyInt 44), ("kamaz", PropertyText "ahaha")]
      entities <- SEP.selectEntities conn "entity2" All
      assertEqual "Unexpected ids of entitities of second type" [1, 2] $
        sort $ map entityId entities

nextEntityId :: Test
nextEntityId =
  TestLabel "Next entity id" $
  TestCase $ do
    databaseFile <- emptySystemTempFile "database"
    SQLite.withConnection databaseFile $ \conn -> do
      SEP.prepareSchema conn
      id1 <- SEP.nextEntityId conn "entity"
      id2 <- SEP.nextEntityId conn "entity1"
      id3 <- SEP.nextEntityId conn "entity1"
      assertEqual "Unexpected id" 1 id1
      assertEqual "Unexpected id" 1 id2
      assertEqual "Unexpected id" 2 id3

selectEntitiesWithPropertyEquals :: Test
selectEntitiesWithPropertyEquals =
  TestLabel "Select Entities with PropertyEquals" $
  TestCase $ do
    databaseFile <- emptySystemTempFile "database"
    SQLite.withConnection databaseFile $ \conn -> do
      SEP.prepareSchema conn
      replicateM_ 2 $
        SEP.createEntity conn "entity" $ M.fromList [("foo", PropertyInt 42)]
      replicateM_ 3 $
        SEP.createEntity conn "entity" $ M.fromList [("foo", PropertyInt 44)]
      entities <-
        SEP.selectEntities
          conn
          "entity"
          (Filter (PropertyEquals "foo" (PropertyInt 42)) All)
      assertEqual "Unexpected amount of entities selected" 2 $ length entities

deleteEntitiesWithPropertyEquals :: Test
deleteEntitiesWithPropertyEquals =
  TestLabel "Delete Entities with PropertyEquals" $
  TestCase $ do
    databaseFile <- emptySystemTempFile "database"
    SQLite.withConnection databaseFile $ \conn -> do
      SEP.prepareSchema conn
      replicateM_ 2 $
        SEP.createEntity conn "entity" $ M.fromList [("foo", PropertyInt 42)]
      replicateM_ 3 $
        SEP.createEntity conn "entity" $ M.fromList [("foo", PropertyInt 43)]
      countRemoved <-
        SEP.deleteEntities
          conn
          "entity"
          (Filter (PropertyEquals "foo" (PropertyInt 42)) All)
      countRemaining <- length <$> SEP.selectEntities conn "entity" All
      assertEqual "Unexpected amount of entities removed" 2 countRemoved
      assertEqual "Unexpected amount of remaining entities" 3 countRemaining

getRandomEntityIdWithPropertyEquals :: Test
getRandomEntityIdWithPropertyEquals =
  TestLabel "Get random entity id with property equals to a value" $
  TestCase $
  withTempSqliteDatabase $ \conn -> do
    SEP.prepareSchema conn
           -- The amount of entities that should not be selected. If
           -- the test is broken, the probably of a false positive
           -- result of this test is equal to 1 / wrongEntitiesCount.
    let wrongEntitiesCount = 1000
    replicateM_ wrongEntitiesCount $
      SEP.createEntity conn "entity" $ M.fromList [("foo", PropertyInt 42)]
    createdEntity <-
      SEP.createEntity conn "entity" $ M.fromList [("foo", PropertyInt 43)]
    randomEntity <-
      SEP.selectEntities
        conn
        "entity"
        (Take 1 $ Shuffle $ Filter (PropertyEquals "foo" (PropertyInt 43)) All)
    assertEqual
      "Unexpected random entity selected"
      (return createdEntity)
      randomEntity
