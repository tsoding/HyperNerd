module Entity where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time

data Property = PropertyInt Int
              | PropertyText T.Text
              | PropertyUTCTime UTCTime
                deriving (Eq, Show)

data Entity = Entity { entityName :: T.Text
                     , entityProperties :: M.Map T.Text Property
                     } deriving (Eq, Show)
