SELECT 
    DB_NAME() AS DatabaseName, 
    SCHEMA_NAME(this.schema_id) AS SchemaName, 
    this.name AS ViewName, 
    last_user_lookups = MAX(ISNULL(views_stats.last_user_lookup, 0))
FROM sys.views AS this
LEFT JOIN sys.dm_db_index_usage_stats AS views_stats
    ON views_stats.object_id = this.object_id
    AND views_stats.database_id = DB_ID()
GROUP BY SCHEMA_NAME(this.schema_id), this.name
ORDER BY last_user_lookups;


select  top 10 * from sys.dm_db_index_usage_stats

select top 10 * from sys.views