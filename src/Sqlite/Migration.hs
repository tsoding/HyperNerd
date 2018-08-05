{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sqlite.Migration ( migrateDatabase
                        , Migration(..)
                        ) where

import           Data.Foldable
import           Data.List
import           Data.String
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Text.RawString.QQ
import           Text.Printf

newtype Migration = Migration { migrationQuery :: Query } deriving Eq

instance IsString Migration where
    fromString = Migration . fromString

instance FromRow Migration where
    fromRow = fromString <$> field

createMigrationTablesIfNeeded :: Connection -> IO ()
createMigrationTablesIfNeeded conn =
    execute_ conn [r| CREATE TABLE IF NOT EXISTS Migrations (
                        id INTEGER PRIMARY KEY,
                        migrationQuery TEXT NOT NULL
                      ) |]

filterUnappliedMigrations :: Connection -> [Migration] -> IO [Migration]
filterUnappliedMigrations conn migrations =
    do appliedMigrations <- query_ conn [r| SELECT migrationQuery
                                            FROM Migrations |]
       maybe (error "Inconsistent migrations state! \
                    \List of already applied migrations \
                    \is not a prefix of required migrations.")
             return
             (stripPrefix appliedMigrations migrations)

applyMigration :: Connection -> Migration -> IO ()
applyMigration conn migration =
    do printf "Applying migration: %s\n" $ fromQuery $ migrationQuery $ migration
       execute_ conn $ migrationQuery migration
       executeNamed conn
                    [r| INSERT INTO Migrations (
                          migrationQuery
                        ) VALUES (
                          :migrationQuery
                        ) |]
                    [ ":migrationQuery" := (fromQuery $ migrationQuery migration :: T.Text) ]

migrateDatabase :: Connection -> [Migration] -> IO ()
migrateDatabase conn migrations =
    do createMigrationTablesIfNeeded conn
       unappliedMigrations <- filterUnappliedMigrations conn migrations
       traverse_ (applyMigration conn) unappliedMigrations
