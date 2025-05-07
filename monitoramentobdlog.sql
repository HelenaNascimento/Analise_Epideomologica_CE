DECLARE @LogSizeMB FLOAT, @LogUsedMB FLOAT, @LogUsedPercent FLOAT;

-- Obtém informações sobre o tamanho do log
SELECT 
    @LogSizeMB = total_log_size_in_bytes / 1024.0 / 1024.0,  -- Converte para MB
    @LogUsedMB = used_log_space_in_bytes / 1024.0 / 1024.0,  -- Converte para MB
    @LogUsedPercent = (used_log_space_in_bytes * 100.0) / total_log_size_in_bytes  -- Percentual usado
FROM sys.dm_db_log_space_usage;

-- Se o log estiver acima de 70% de uso, dispara um alerta e envia o resultado do sp_whoisactive por e-mail
IF @LogUsedPercent >= 70
BEGIN
    DECLARE @Mensagem NVARCHAR(1000);
    SET @Mensagem = '🚨 ALERTA: O log do banco de dados DMD atingiu ' + 
                    CAST(@LogUsedPercent AS NVARCHAR(10)) + '% de uso!';

    -- Tabela temporária para armazenar o resultado do sp_whoisactive
    IF OBJECT_ID('tempdb..#whoisactive') IS NOT NULL DROP TABLE #whoisactive;

    CREATE TABLE #whoisactive (
        [dd hh:mm:ss.mss] VARCHAR(20),
        session_id SMALLINT,
        sql_text NVARCHAR(MAX),
        login_name NVARCHAR(100),
        wait_info NVARCHAR(100),
        CPU VARCHAR(30),
        tempdb_allocations VARCHAR(30),
        tempdb_current VARCHAR(30),
        blocking_session_id SMALLINT,
        used_memory VARCHAR(30),
        status NVARCHAR(30),
        open_tran_count INT,
        percent_complete FLOAT,
        host_name NVARCHAR(100),
        database_name NVARCHAR(100),
        program_name NVARCHAR(100),
        start_time DATETIME,
        login_time DATETIME,
        request_id INT,
        collection_time DATETIME,
        additional_info XML -- ou outros campos que você queira capturar
    );

    -- Executa sp_whoisactive e grava na temp table
    EXEC sp_whoisactive 
        @get_outer_command = 1, 
        @get_full_inner_text = 1, 
        @destination_table = '#whoisactive';

    -- Envia e-mail com alerta
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'AlertaSQL',  
        @recipients = 'silvania@novadistribuidorane.com.br',
        @subject = '⚠ ALERTA: Log do SQL Server Acima de 70%',
        @body = @Mensagem;

    -- Envia o resultado do whoisactive como anexo
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'AlertaSQL',
        @recipients = 'silvania@novadistribuidorane.com.br',
        @subject = '📋 Diagnóstico: sp_whoisactive após alerta de log',
        @body = 'Segue em anexo o resultado do sp_whoisactive no momento do alerta de log.',
        @query = 'SELECT * FROM #whoisactive',
        @attach_query_result_as_file = 1,
        @query_attachment_filename = 'whoisactive_log_alert.txt',
        @query_result_separator = '|';

    -- (Opcional) Disparar um job se desejar
    EXEC msdb.dbo.sp_start_job @job_name = 'Monitor_Log_1';
END
