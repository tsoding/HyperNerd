# Selector Compilation

Example:

```sql
with
t1(eid) as (                    -- All
   select entityId
   from entityproperty
   where entityName is "LogRecord"
   group by entityId
   limit 100
),
t2(eid) as (                    -- Filter
   select eid
   from t1
   where exists (select *
                 from entityproperty
                 where entityId is eid
                   and propertyName = "user"
                   and propertyInt IS NULL
                   and propertyText IS "tsoding"
                   and propertyUTCTime IS NULL)
),
t3(eid) as (                    -- Take 5
   select eid
   from t2
   order by eid ASC
   limit 5
),
t4(eid) as (                    -- SortTextBy "msg"
   select eid
   from t3
   order by (select propertyText
             from entityproperty
             where entityId is eid
               and propertyName is "msg") ASC
)
select propertyText
from EntityProperty
inner join t4 on entityId = t4.eid
where propertyName is "msg";
```
