DECLARE 
    @IdView INT,
    @dat_lookup DATETIME,
    @nome VARCHAR(50);

-- Cursor para listar views
DECLARE nom_view CURSOR FOR
    SELECT
        v.object_id,
        v.name
    FROM sys.views AS v
OPEN nom_view;

FETCH NEXT FROM nom_view INTO @IdView, @nome;
WHILE @@FETCH_STATUS = 0
BEGIN
    -- Pega a última data de lookup de cada view na tabela de stats
    SELECT TOP 1 
        @dat_lookup = MAX(d.last_user_lookup)
    FROM sys.dm_db_index_usage_stats AS d
    WHERE d.object_id = @IdView
		--and last_user_lookup is not null
    GROUP BY d.object_id;

    -- Imprime o resultado
    PRINT CAST(@IdView AS NVARCHAR(255)) + ';' + CAST(@nome AS NVARCHAR(255)) + ';' + COALESCE(CAST(@dat_lookup AS NVARCHAR(255)), 'Sem dados');

    -- Próxima view
    FETCH NEXT FROM nom_view INTO @IdView, @nome;
END

-- Fecha e deleta o cursor
CLOSE nom_view;
DEALLOCATE nom_view;
