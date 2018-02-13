module Entity where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time

data Property = PropertyInt Int
              | PropertyText T.Text
              | PropertyUTCTime UTCTime
                deriving (Eq, Show)

type Properties = M.Map T.Text Property

data Entity = Entity { entityId :: Int
                     , entityName :: T.Text
                     , entityProperties :: M.Map T.Text Property
                     } deriving (Eq, Show)

propertyTypeName :: Property -> String
propertyTypeName (PropertyInt _) = "PropertyInt"
propertyTypeName (PropertyText _) = "PropertyText"
propertyTypeName (PropertyUTCTime _) = "PropertyUTCTime"

propertyAsInt :: Property -> Maybe Int
propertyAsInt (PropertyInt x) = Just x
propertyAsInt _ = Nothing

propertyAsText :: Property -> Maybe T.Text
propertyAsText (PropertyText x) = Just x
propertyAsText _ = Nothing

propertyAsUTCTime :: Property -> Maybe UTCTime
propertyAsUTCTime (PropertyUTCTime x) = Just x
propertyAsUTCTime _ = Nothing
