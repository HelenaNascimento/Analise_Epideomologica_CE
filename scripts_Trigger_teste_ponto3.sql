-- ⚡ 1. Ativar Database Mail (só precisa uma vez)
EXEC sp_configure 'show advanced options', 1;
RECONFIGURE;
EXEC sp_configure 'Database Mail XPs', 1;
RECONFIGURE;

-- ⚡ 2. Criar um perfil de e-mail no Database Mail (ajustar com seus dados de SMTP)
EXEC msdb.dbo.sysmail_add_account_sp
    @account_name = 'ContaPonto',
    @description  = 'Conta de envio de confirmações de ponto',
    @email_address= 'seuemail@empresa.com',
    @display_name = 'Sistema de Ponto',
    @mailserver_name = 'smtp.seuservidor.com',
    @username = 'usuarioSMTP',
    @password = 'senhaSMTP',
    @port = 587, -- ou 25/465 dependendo do provedor
    @enable_ssl = 1;

EXEC msdb.dbo.sysmail_add_profile_sp
    @profile_name = 'PerfilPonto',
    @description = 'Perfil para envio de confirmação de ponto';

EXEC msdb.dbo.sysmail_add_profileaccount_sp
    @profile_name = 'PerfilPonto',
    @account_name = 'ContaPonto',
    @sequence_number = 1;

EXEC msdb.dbo.sysmail_add_principalprofile_sp
    @profile_name = 'PerfilPonto',
    @principal_name = 'public',
    @is_default = 1;

-- ⚡ 3. Criar Trigger
CREATE TRIGGER trg_EnviarEmailBatida
ON dbo.BatidaPonto
AFTER INSERT
AS
BEGIN
    SET NOCOUNT ON;

    DECLARE @FuncionarioEmail NVARCHAR(200);
    DECLARE @FuncionarioNome NVARCHAR(100);
    DECLARE @DataHora DATETIME;

    -- Supondo que a tabela tenha os campos IdFuncionario, DataHoraBatida
    SELECT 
        @FuncionarioEmail = f.Email,
        @FuncionarioNome  = f.Nome,
        @DataHora         = i.DataHoraBatida
    FROM inserted i
    JOIN Funcionarios f ON f.IdFuncionario = i.IdFuncionario;

    -- Envio do email
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'PerfilPonto',
        @recipients = @FuncionarioEmail,
        @subject = 'Confirmação de Ponto',
        @body = 'Olá ' + @FuncionarioNome 
              + ', sua batida de ponto foi registrada em ' 
              + CONVERT(VARCHAR(19), @DataHora, 120) + '.';
END;
GO


DECLARE @FuncionarioEmail NVARCHAR(200);
DECLARE @FuncionarioNome NVARCHAR(100);
DECLARE @DataHora DATETIME;

SELECT TOP 1
    @FuncionarioEmail = EPG.eMail,
    @FuncionarioNome  = EPG.Nome,
    @DataHora         = btp.DataHora
FROM EPG
OUTER APPLY (
    SELECT TOP 1 DataHora
    FROM BTP b
    WHERE b.EMP_Codigo = EPG.EMP_Codigo 
      AND b.EPG_Codigo = EPG.Codigo
      AND CONVERT(date, b.DataHora) = CONVERT(date, GETDATE())
    ORDER BY b.DataHora ASC
) btp
WHERE EPG.EMP_Codigo = '0112';

SELECT @FuncionarioEmail, @FuncionarioNome, @DataHora;


USE [Ponto3]
GO
/****** Object:  Trigger [dbo].[trg_EnviarEmailBatida]    Script Date: 20/08/2025 15:28:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER TRIGGER [dbo].[trg_EnviarEmailBatida]
ON [dbo].[BTP]
AFTER INSERT
AS

BEGIN
    SET NOCOUNT ON;

    DECLARE @FuncionarioEmail NVARCHAR(200);
    DECLARE @FuncionarioNome NVARCHAR(100);
    DECLARE @DataHora DATETIME;

    -- Supondo que a tabela tenha os campos IdFuncionario, DataHoraBatida
    SELECT 
        @FuncionarioEmail = EPG.eMail,
        @FuncionarioNome  = EPG.Nome,
        @DataHora         = btp.DataHora
	FROM BTP BTP
			JOIN EPG EPG ON BTP.EMP_Codigo = EPG.EMP_Codigo AND BTP.EPG_Codigo = EPG.Codigo
		where 
		BTP.EMP_Codigo = '0112'
		AND format(btp.DataHora, 'MM/dd/yyyy') = format(getdate(), 'MM/dd/yyyy') 

    DECLARE @Mensagem NVARCHAR(1000);
    SET @Mensagem = 'Olá ' + @FuncionarioNome  + ', sua batida de ponto foi registrada em ' + CONVERT(VARCHAR(19), @DataHora) +'.';

    -- Envio do email
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'PerfilPonto',
        @recipients = @FuncionarioEmail,
        @subject = 'Confirmação de Registro de Ponto',
        @body = @Mensagem;
END;

-- Cria a tabela temporária
IF OBJECT_ID('tempdb..#BatidasHoje') IS NOT NULL
    DROP TABLE #BatidasHoje;

CREATE TABLE #BatidasHoje (
    FuncionarioEmail NVARCHAR(200),
    FuncionarioNome NVARCHAR(100),
    DataHora DATETIME
);

-- Insere o resultado
INSERT INTO #BatidasHoje (FuncionarioEmail, FuncionarioNome, DataHora)
SELECT 
    top 1
    EPG.eMail,
    EPG.Nome,
    MIN(btp.DataHora) AS DataHora
FROM BTP btp
JOIN EPG EPG 
    ON btp.EMP_Codigo = EPG.EMP_Codigo 
   AND btp.EPG_Codigo = EPG.Codigo
WHERE 
    btp.EMP_Codigo = '0112'

    AND CONVERT(date, btp.DataHora) = CONVERT(date, GETDATE())
GROUP BY EPG.eMail, EPG.Nome;
ORDER BY MIN(btp.DataHora);

-- Conferir o que foi gravado
SELECT * FROM #BatidasHoje;

DECLARE @FuncionarioEmail NVARCHAR(200);
DECLARE @FuncionarioNome NVARCHAR(100);
DECLARE @DataHora DATETIME;

SELECT TOP 1
    @FuncionarioEmail = EPG.eMail,
    @FuncionarioNome  = EPG.Nome,
    @DataHora         = MIN(btp.DataHora)
FROM BTP btp
JOIN EPG EPG 
    ON btp.EMP_Codigo = EPG.EMP_Codigo 
   AND btp.EPG_Codigo = EPG.Codigo
WHERE 
    btp.EMP_Codigo = '0112'
    AND CONVERT(date, btp.DataHora) = CONVERT(date, GETDATE())
GROUP BY EPG.eMail, EPG.Nome
    

SELECT @FuncionarioEmail, @FuncionarioNome, @DataHora;


USE [Ponto3]
GO
/****** Object:  Trigger [dbo].[trg_EnviarEmailBatida]    Script Date: 20/08/2025 15:28:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

ALTER TRIGGER [dbo].[trg_EnviarEmailBatida]
ON [dbo].[BTP]
AFTER INSERT
AS

BEGIN
    SET NOCOUNT ON;

    DECLARE @FuncionarioEmail NVARCHAR(200);
    DECLARE @FuncionarioNome NVARCHAR(100);
    DECLARE @DataHora DATETIME;

    -- Supondo que a tabela tenha os campos IdFuncionario, DataHoraBatida
    SELECT 
        @FuncionarioEmail = EPG.eMail,
        @FuncionarioNome  = EPG.Nome,
        @DataHora         = btp.DataHora
	FROM BTP BTP
			JOIN EPG EPG ON BTP.EMP_Codigo = EPG.EMP_Codigo AND BTP.EPG_Codigo = EPG.Codigo
		where 
		BTP.EMP_Codigo = '0112'
		AND format(btp.DataHora, 'MM/dd/yyyy') = format(getdate(), 'MM/dd/yyyy') 

    DECLARE @Mensagem NVARCHAR(1000);
    SET @Mensagem = 'Olá ' + @FuncionarioNome  + ', sua batida de ponto foi registrada em ' + CONVERT(VARCHAR(19), @DataHora) +'.';

    -- Envio do email
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'PerfilPonto',
        @recipients = @FuncionarioEmail,
        @subject = 'Confirmação de Registro de Ponto',
        @body = @Mensagem;
END;


SELECT * FROM msdb.dbo.sysmail_account;



EXEC msdb.dbo.sysmail_update_account_sp
    @account_name    = 'ContaPonto',  -- nome da conta já criada
    @description     = 'Conta de envio atualizada',
    @email_address   = 'silvania@novadistribuidorane.com.br',
    @display_name    = 'Sistema de Ponto',
    @mailserver_name = 'smtp.office365.com',
    @username        = 'silvania@novadistribuidorane.com.br',
    @password        = 'gpnova@2025',
    @port            = 587,
    @enable_ssl      = 1;


EXEC msdb.dbo.sp_send_dbmail
    @profile_name = 'PerfilPonto',
    @recipients   = 'silvania@novadistribuidorane.com.br',
    @subject      = 'Teste de envio',
    @body         = 'Este é um teste do Database Mail após atualização.';


SELECT * 
FROM msdb.dbo.sysmail_allitems 
ORDER BY send_request_date DESC;


select 
	DISTINCT
	btp.DataHora,
	EPG.Nome,
	EPG.eMail
	from BTP BTP
		JOIN EPG EPG ON BTP.EMP_Codigo = EPG.EMP_Codigo AND BTP.EPG_Codigo = EPG.Codigo
		--JOIN (SELECT max(btp.DataHora) datahora FROM btp ) hr_atual on BTP.DataHora = hr_atual.datahora
	where 
	BTP.EMP_Codigo = '0112'
	AND format(btp.DataHora, 'MM/dd/yyyy') = format(getdate(), 'MM/dd/yyyy') 
	order by 1 desc

select 
	DISTINCT
	btp.DataHora,
	EPG.Nome,
	EPG.eMail
	from BTP BTP
		JOIN EPG EPG ON BTP.EMP_Codigo = EPG.EMP_Codigo AND BTP.EPG_Codigo = EPG.Codigo
		--JOIN (SELECT max(btp.DataHora) datahora FROM btp ) hr_atual on BTP.DataHora = hr_atual.datahora
	where 
	BTP.EMP_Codigo = '0112'
	AND format(btp.DataHora, 'MM/dd/yyyy') = format(getdate(), 'MM/dd/yyyy') 
	order by 1 desc

