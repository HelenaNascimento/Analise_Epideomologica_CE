--ATIVAR DATABASE EMAIL
EXEC sp_configure 'show advanced options', 1;
RECONFIGURE;
EXEC sp_configure 'Database Mail XPs', 1;
RECONFIGURE;


--CRIAR PERFIL DE EMAIL
EXEC msdb.dbo.sysmail_add_profile_sp
    @profile_name = 'AlertaSQL',
    @description = 'Perfil de e-mail para alertas do SQL Server';

--SELECT * FROM msdb.dbo.sysmail_profile;

--VINCULAR UM EMAIL
EXEC msdb.dbo.sysmail_add_account_sp
    @account_name = 'ContaAlertaSQL',
    @description = 'Conta de e-mail para alertas',
    @email_address = 'silvania@novadistribuidorane.com.br',
    @display_name = 'SQL Server Alerta',
    @mailserver_name = 'outlook.office365.com',  -- Servidor SMTP
    @port = 587,  -- Porta SMTP (Gmail usa 587)
    @username = 'silvania@novadistribuidorane.com.br',
    @password = 'gpnova#20261',  -- ⚠ NÃO é recomendado salvar a senha aqui diretamente
    @enable_ssl = 1;  -- Ativa criptografia SSL

--ASSOCIAR CONTA AO PERFIL
EXEC msdb.dbo.sysmail_add_profileaccount_sp
    @profile_name = 'AlertaSQL',
    @account_name = 'ContaAlertaSQL',
    @sequence_number = 1;

--SELECT 
--    p.name AS ProfileName, 
--    a.name AS AccountName
--FROM msdb.dbo.sysmail_profile p
--LEFT JOIN msdb.dbo.sysmail_profileaccount pa ON p.profile_id = pa.profile_id
--LEFT JOIN msdb.dbo.sysmail_account a ON pa.account_id = a.account_id;

--**********************************************---
--CRIAR UM OPERADOR DE NOTIFICAÇÃO
EXEC msdb.dbo.sp_add_operator
    @name = 'AdminSQL',
    @enabled = 1,
    @email_address = 'outlook.office365.com';

--**********************************************---
--CRIAR UM JOB CHAMADO "Monitorar Uso do Log"
--Adicionar um novo Step e escolher "Tipo: Transact-SQL (T-SQL)"

DECLARE @LogSizeMB FLOAT, @LogUsedMB FLOAT, @LogUsedPercent FLOAT;

-- Obtém informações sobre o tamanho do log
SELECT 
    @LogSizeMB = total_log_size_in_bytes / 1024.0 / 1024.0,  -- Converte para MB
    @LogUsedMB = used_log_space_in_bytes / 1024.0 / 1024.0,  -- Converte para MB
    @LogUsedPercent = (used_log_space_in_bytes * 100.0) / total_log_size_in_bytes  -- Percentual usado
FROM sys.dm_db_log_space_usage;

-- Se o log estiver acima de 80% de uso, dispara um alerta por e-mail
IF @LogUsedPercent >= 80
BEGIN
    DECLARE @Mensagem NVARCHAR(1000);
    SET @Mensagem = '🚨 ALERTA: O log do banco de dados DMD atingiu ' + 
                    CAST(@LogUsedPercent AS NVARCHAR(10)) + '% de uso!';

    -- Envia e-mail com alerta
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'AlertaSQL',  
        @recipients = 'seuemail@dominio.com',
        @subject = '⚠ ALERTA: Log do SQL Server Acima de 80%',
        @body = @Mensagem;
END

--**********************************************---
-- VERIFICAR O MODELO DE RECUPERAÇÃO
-- SE TIVER FULL PASSA PARA SIMPLE
IF (SELECT recovery_model_desc FROM sys.databases WHERE name = 'BD_BRIND') ='FULL'
ALTER DATABASE BD_BRIND SET RECOVERY SIMPLE;
GO

--**********************************************---
--REDUZIR O LOG SEM IMPACTAR NO BANCO
USE DMD;
GO
-- Definir um tamanho mínimo razoável (exemplo: 512MB)
DECLARE @MinLogSizeMB INT = 512;

-- Calcular o tamanho atual do log
DECLARE @LogFileName NVARCHAR(100);
DECLARE @CurrentLogSizeMB INT;

SELECT @LogFileName = name, @CurrentLogSizeMB = size / 128 
FROM sys.master_files 
WHERE database_id = DB_ID('DMD') AND type_desc = 'LOG';

-- Se o log for maior que 1GB, reduzimos para um valor seguro
IF @CurrentLogSizeMB > 1024
BEGIN
    DBCC SHRINKFILE (@LogFileName, @MinLogSizeMB);
END
GO

--**********************************************---
--SE ANTES DA EXECUCAO ESTAVA FULL ENTÃO DEVERÁ RETORNAR AO MODELO ORIGINAL
ALTER DATABASE DMD SET RECOVERY FULL;
GO
BACKUP DATABASE DMD TO DISK = 'C:\Backup\DMD_FULL.bak'; --ALTERAR O CAMINHO
GO
--**********************************************---