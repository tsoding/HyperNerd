{-# LANGUAGE OverloadedStrings #-}
module SqliteEntityPersistenceSpec where

import           Data.List
import qualified Data.Map as M
import qualified Database.SQLite.Simple as SQLite
import           Effect (Selector(All))
import           Entity
import qualified SqliteEntityPersistence as SEP
import           System.IO.Temp
import           Test.HUnit

doublePrepareSchemaSpec :: Test
doublePrepareSchemaSpec =
    TestLabel "Double Prepare SEP Schema" $
    TestCase $ do databaseFile <- emptySystemTempFile "database"
                  SQLite.withConnection databaseFile SEP.prepareSchema
                  SQLite.withConnection databaseFile SEP.prepareSchema

createEntityAndGetItById :: Test
createEntityAndGetItById =
    TestLabel "Create an Entity and get it by id" $
    TestCase $ do databaseFile <- emptySystemTempFile "database"
                  SQLite.withConnection databaseFile $ \conn ->
                      do SEP.prepareSchema conn
                         storedEntity   <- SEP.createEntity conn "entity"
                                           $ M.fromList [ ("foo", PropertyInt 42)
                                                        , ("bar", PropertyText "hello")
                                                        ]
                         restoredEntity <- SEP.getEntityById conn "entity" $ entityId storedEntity
                         assertEqual "Unexpected restored entity" (Just storedEntity) restoredEntity

createSeveralEntityTypes :: Test
createSeveralEntityTypes =
    TestLabel "Create several Entity types" $
    TestCase $ do databaseFile <- emptySystemTempFile "database"
                  SQLite.withConnection databaseFile $ \conn ->
                      do SEP.prepareSchema conn
                         _ <- SEP.createEntity conn "entity1"
                                $ M.fromList [ ("foo", PropertyInt 42)
                                             , ("bar", PropertyText "hello")
                                             ]
                         _ <- SEP.createEntity conn "entity2"
                                $ M.fromList [ ("baz", PropertyInt 43)
                                             , ("kamaz", PropertyText "world")
                                             ]
                         _ <- SEP.createEntity conn "entity2"
                                $ M.fromList [ ("baz", PropertyInt 44)
                                             , ("kamaz", PropertyText "ahaha")
                                             ]
                         entities <- SEP.selectEntities conn "entity2" All
                         assertEqual "Unexpected ids of entitities of second type"[1, 2] $ sort $ map entityId entities
