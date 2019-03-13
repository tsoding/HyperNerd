.output markov.txt
select ep1.propertyText
from EntityProperty ep1
where ep1.entityName = 'LogRecord'
  and ep1.propertyName = 'msg'
