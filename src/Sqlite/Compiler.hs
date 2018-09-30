{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sqlite.Compiler (compileSelector, NamedQuery) where

import           Data.Monoid((<>))
import qualified Data.Text as T
import           Data.Time
import           Database.SQLite.Simple
import qualified Effect as E
import           Property
import           Text.InterpolatedString.QM

type NamedQuery = (Query, [NamedParam])

header :: NamedQuery
header = ("with ", [])

footer :: Int -> NamedQuery
footer cteId = (Query [qm|\ select * from t{cteId}|], [])

compileCondition :: Int -> E.Condition -> NamedQuery
compileCondition cteId (E.PropertyEquals propertyName property) =
    ([qms| select *
           from EntityProperty
           where entityId is eid
             and propertyName = :propertyName{cteId}
             and propertyInt is :propertyIntValue{cteId}
             and propertyText is :propertyTextValue{cteId}
             and propertyUTCTime is :propertyUTCTimeValue{cteId}
         |],
     [ [qm| :propertyName{cteId} |] := propertyName
     , [qm| :propertyIntValue{cteId} |] := (fromProperty property :: Maybe Int)
     , [qm| :propertyTextValue{cteId} |] := (fromProperty property :: Maybe T.Text)
     , [qm| :propertyUTCTimeValue{cteId} |] := (fromProperty property :: Maybe UTCTime)
     ])

-- TODO(#253): compileCteChain doesn't optimize common patterns like Sqlite.EntityPersistence.selectEntityIds
compileCteChain :: T.Text -> E.Selector -> (Int, NamedQuery)
compileCteChain name E.All =
    (0, ( [qms| t0(eid) as (
                 select entityId
                 from EntityProperty
                 where entityName is :entityName0
                 group by entityId
               ) |]
        , [":entityName0" := name ]
        ))
compileCteChain name (E.Filter condition input) =
    (filterCteId, ( [qms| t{filterCteId}(eid) as (
                            select eid
                            from t{inputCteId}
                            where exists ({conditionQuery})
                          ), |]
                  , conditionParams ) <> inputQuery)
    where (inputCteId, inputQuery) = compileCteChain name input
          filterCteId = inputCteId + 1
          (conditionQuery, conditionParams) = compileCondition filterCteId condition
compileCteChain name (E.Shuffle input) =
    (shuffleCteId, ( [qms| t{shuffleCteId}(eid) as (
                             select eid
                             from t{inputCteId}
                             order by random()
                           ), |]
                   , [] ) <> inputQuery)
    where (inputCteId, inputQuery) = compileCteChain name input
          shuffleCteId = inputCteId + 1
compileCteChain name (E.Take n input) =
    (takeCteId, ( [qms| t{takeCteId}(eid) as (
                          select eid
                          from t{inputCteId}
                          order by eid asc
                          limit :n{takeCteId}
                        ), |]
                   , [ [qm| :n{takeCteId} |] := n ] ) <> inputQuery)
    where (inputCteId, inputQuery) = compileCteChain name input
          takeCteId = inputCteId + 1
-- TODO(#254): compileCteChain for SortBy only supports propertyUTCTime
compileCteChain name (E.SortBy propertyName order input) =
    (sortByCteId, ( [qms| t{sortByCteId}(eid) as (
                            select eid
                            from t{inputCteId}
                            order by (select propertyUTCTime
                                      from EntityProperty
                                      where entityId is eid
                                        and propertyName is :propertyName{sortByCteId}) {order}
                          ), |]
                   , [ [qm| :propertyName{sortByCteId} |] := propertyName ] ) <> inputQuery)
    where (inputCteId, inputQuery) = compileCteChain name input
          sortByCteId = inputCteId + 1

compileSelector :: T.Text -> E.Selector -> NamedQuery
compileSelector name selector =
    header <> body <> footer cteId
    where (cteId, body) = compileCteChain name selector
