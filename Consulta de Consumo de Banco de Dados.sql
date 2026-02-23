SELECT 
    t.name AS Tabela,
    i.name AS Indice,
    avg_fragmentation_in_percent AS Porcentagem_Fragmentacao,
    page_count AS Total_Paginas
FROM sys.dm_db_index_physical_stats(DB_ID(), NULL, NULL, NULL, 'DETAILED') AS s
JOIN sys.indexes AS i ON s.object_id = i.object_id AND s.index_id = i.index_id
JOIN sys.tables AS t ON i.object_id = t.object_id
WHERE avg_fragmentation_in_percent > 5 -- Filtra índices com fragmentação mínima
ORDER BY avg_fragmentation_in_percent DESC;