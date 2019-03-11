.output log.txt
select ep3.propertyUTCTime, ep1.propertyText, ep2.propertyText
from EntityProperty ep1
inner join EntityProperty ep2 on (ep1.entityId = ep2.entityId and ep1.entityName = ep2.entityName)
inner join EntityProperty ep3 on (ep1.entityId = ep3.entityId and ep1.entityName = ep3.entityName)
where ep1.entityName = 'LogRecord'
  and ep1.propertyName = 'user'
  and ep2.propertyName = 'msg'
  and ep3.propertyName = 'timestamp';
