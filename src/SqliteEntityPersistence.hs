module SqliteEntityPersistence where

import qualified Data.Text as T
import           Database.SQLite.Simple
import           Entity

-- TODO(#49): Implement SqliteEntityPersistance module

prepareSchema :: Connection -> IO ()
prepareSchema _ = return ()

saveEntity :: Entity -> IO Int
saveEntity _ = return 42

getEntityById :: T.Text -> Int -> IO (Maybe Entity)
getEntityById _ _ = return Nothing
