{-# LANGUAGE OverloadedStrings #-}
module SqliteEntityPersistenceSpec where

import qualified Data.Map as M
import qualified Database.SQLite.Simple as SQLite
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
