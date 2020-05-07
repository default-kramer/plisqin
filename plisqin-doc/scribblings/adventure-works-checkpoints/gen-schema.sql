-- This is specific to SQL Server
select '(table '
	+ t.TABLE_NAME
	+ ' #:column '
	+ (select '['
			+ c.COLUMN_NAME
			+ case when c.DATA_TYPE like '%int' or c.DATA_TYPE like '%decimal' or c.DATA_TYPE like '%money'
					or c.DATA_TYPE like '%numeric' then ' #:type Number?'
				when c.DATA_TYPE like '%char' then ' #:type String?'
				when c.DATA_TYPE in ('datetime', 'date') then ' #:type Datetime?'
				else '' end
			+ case when c.IS_NULLABLE = 'NO' then ' #:null no'
				else ' #:null yes' end
			+ '] '
			as 'data()'
		from INFORMATION_SCHEMA.COLUMNS c
		where c.TABLE_NAME = t.TABLE_NAME
		and c.TABLE_SCHEMA = t.TABLE_SCHEMA
		order by c.COLUMN_NAME
		for xml path('')
	)
	+ ')' as RacketCode
from INFORMATION_SCHEMA.TABLES t
where t.TABLE_TYPE = 'BASE TABLE'
and t.TABLE_SCHEMA <> 'dbo'
order by t.TABLE_SCHEMA, t.TABLE_NAME