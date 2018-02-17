{-# LANGUAGE OverloadedStrings #-}
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

restoreProperty :: (T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime) -> Maybe (T.Text, Property)
restoreProperty (name, "PropertyInt", Just x, Nothing, Nothing) = Just (name, PropertyInt x)
restoreProperty (name, "PropertyText", Nothing, Just x, Nothing) = Just (name, PropertyText x)
restoreProperty (name, "PropertyUTCTime", Nothing, Nothing, Just x) = Just (name, PropertyUTCTime x)
restoreProperty _ = error "Khooy"

restoreEntity :: T.Text -> Int -> [(T.Text, T.Text, Maybe Int, Maybe T.Text, Maybe UTCTime)] -> Maybe Entity
restoreEntity name ident rawProperties =
    do properties <- sequence $ map restoreProperty rawProperties
       if null properties
       then Nothing
       else Just $ Entity { entityName = name
                          , entityId = ident
                          , entityProperties = M.fromList properties
                          }
