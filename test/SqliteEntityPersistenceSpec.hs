module SqliteEntityPersistenceSpec where

import qualified Database.SQLite.Simple as SQLite
import qualified SqliteEntityPersistence as SEP
import           System.IO.Temp
import           Test.HUnit

doublePrepareSchemaSpec :: Test
doublePrepareSchemaSpec =
    TestLabel "Double Prepare SEP Schema" $
    TestCase $ do databaseFile <- emptySystemTempFile "database"
                  SQLite.withConnection databaseFile SEP.prepareSchema
                  SQLite.withConnection databaseFile SEP.prepareSchema
