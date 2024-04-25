-- CRIACAO DO BANCO DE DADOS Integração AZAPFY
USE [master]
GO

DECLARE @dbname nvarchar(128),
        @dbpath nvarchar(500),
		@sqlcmd nvarchar(1000)

SET @dbname = N'DMD_AZAPFY'
SET @dbpath = N'C:\Infarma\DB\'

-------------------------------------------------------------------------------------'
SET @sqlcmd = 'CREATE DATABASE ['+@dbname+'] ON  PRIMARY 
	           ( NAME = N'''+@dbname+''', FILENAME = N'''+@dbpath+@dbname+'.mdf'' , SIZE = 512MB , MAXSIZE = UNLIMITED, FILEGROWTH = 512MB )
				LOG ON 
			   ( NAME = N'''+@dbname+'_log'', FILENAME = N'''+@dbpath+@dbname+'_log.ldf'' , SIZE = 512MB , MAXSIZE = 100GB , FILEGROWTH = 512MB )'
			   

SET @sqlcmd = @sqlcmd +
    '
	ALTER DATABASE ['+@dbname+'] SET AUTO_CREATE_STATISTICS ON 

	ALTER DATABASE ['+@dbname+'] SET AUTO_SHRINK ON 

	ALTER DATABASE ['+@dbname+'] SET AUTO_UPDATE_STATISTICS ON 

	ALTER DATABASE ['+@dbname+'] SET CURSOR_CLOSE_ON_COMMIT ON

	ALTER DATABASE ['+@dbname+'] SET READ_COMMITTED_SNAPSHOT ON 

	ALTER DATABASE ['+@dbname+'] SET READ_WRITE 

	ALTER DATABASE ['+@dbname+'] SET RECOVERY SIMPLE

	ALTER DATABASE ['+@dbname+'] SET MULTI_USER 
	'
-------------------------------------------------------------------------------------'


IF (NOT EXISTS (SELECT name FROM master.dbo.sysdatabases 
            WHERE ('[' + name + ']' = @dbname OR name = @dbname)))
BEGIN
    PRINT @sqlcmd

	Execute sp_executesql @sqlcmd
END
GO

-------------------------------------------------------------------------------------
Use DMD_AZAPFY
GO

-- maior valor bigint 9223372036854775807 campos chave e cnpj foram trocados por varchar
-- drop table tb_envio
-- Copiando estrutura para tabela Integracao.tb_envio
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tb_envio]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE tb_envio (
  [ID_NOTA] bigint identity(1,1) NOT NULL,
  [CHAVE_NOTA] varchar(44) DEFAULT NULL,
  [NUMERO_NOTA] int NOT NULL,
  [SERIE_NOTA] int NOT NULL,
  [DATA_NOTA] datetime NOT NULL,
  [VALOR_NOTA] numeric(18,4) DEFAULT NULL,
  [REMETENTE_NOME] varchar(50) NOT NULL,
  [REMETENTE_CNPJ] varchar(15) NOT NULL,
  [DESTINATARIO_NOME] varchar(50) NOT NULL,
  [DESTINATARIO_CNPJ] varchar(15) NOT NULL,
  [DESTINATARIO_ENDERECO] varchar(100) NOT NULL,
  [ID_ROMANEIO] int NOT NULL,
  [ROMANEIO] varchar(50) NOT NULL,
  [DATA_ROMANEIO] datetime NOT NULL,
  [MOTORISTA_CPF] int NOT NULL,
  [MOTORISTA_NOME] varchar(50) DEFAULT NULL,
  [PARCEIRO_NOME] varchar(50) DEFAULT NULL,
  [PARCEIRO_CNPJ] varchar(15) DEFAULT NULL,
  [UNIDADE] varchar(50) DEFAULT NULL,
  [VOLUMES] int DEFAULT NULL,
  [STATUS_OPERACAO] varchar(50) DEFAULT NULL,
  [STATUS_IMPORTACAO_NOTA] bit DEFAULT 0,
  [STATUS_IMPORTACAO_ROMANEIO] bit DEFAULT 0,
  CONSTRAINT [ID_NOTA] UNIQUE  ([ID_NOTA])
) ;
GO

-- Exportação de dados foi desmarcado.
-- Copiando estrutura para tabela Integracao.tb_retorno
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tb_retorno]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE tb_retorno (
  [ID_NOTA] bigint DEFAULT NULL,
  [OCORRENCIA] varchar(50) DEFAULT NULL,
  [DATA_OPERACAO] datetime DEFAULT NULL,
  [DATA_RECEBIMENTO] datetime DEFAULT NULL
) ;
GO

Declare @sql nvarchar(max)

Select top 1 @sql = 'ALTER TABLE tb_envio DROP CONSTRAINT ' + a.name
from sys.default_constraints a
Join sys.columns b  ON b.column_id = a.parent_column_id
Where b.object_id = OBJECT_ID('tb_envio') 
and b.name = 'VALOR_NOTA'

IF @@ROWCOUNT > 0
  EXECUTE sp_executesql @sql

IF EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'VALOR_NOTA'
			   And Data_Type = 'int')
	Alter table dbo.tb_envio Drop Column VALOR_NOTA
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'VALOR_NOTA')
	Alter table dbo.tb_envio Add VALOR_NOTA [numeric](18, 4)
GO

if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tb_logint]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
	Create Table tb_logint(
	id bigint identity(1,1) not null,
	dt_registro datetime DEFAULT getdate(),
	tx_registro varchar(100),
	CONSTRAINT [pk_tb_logint] UNIQUE  ([id])
	)
GO


IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_LOGRADOURO')
	Alter table dbo.tb_envio Add DESTINATARIO_LOGRADOURO varchar(35) NULL
Go

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_NUMERO')
	Alter table dbo.tb_envio Add DESTINATARIO_NUMERO varchar(5) NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_BAIRRO')
	Alter table dbo.tb_envio Add DESTINATARIO_BAIRRO varchar(20) NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_CEP')
	Alter table dbo.tb_envio Add DESTINATARIO_CEP varchar(8) NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_CIDADE')
	Alter table dbo.tb_envio Add DESTINATARIO_CIDADE varchar(25) NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_CODIGO_CIDADE')
	Alter table dbo.tb_envio Add DESTINATARIO_CODIGO_CIDADE varchar(7)
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DESTINATARIO_UF')
	Alter table dbo.tb_envio Add DESTINATARIO_UF varchar(2) NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'DATA_REGISTRO')
	Alter table dbo.tb_envio Add DATA_REGISTRO datetime NULL
GO

--solicitação azapfy x nova pe

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'MOTORISTA_PLACA')
	Alter table dbo.tb_envio Add MOTORISTA_PLACA varchar(50) DEFAULT NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'STATUS_NF')
	Alter table dbo.tb_envio Add STATUS_NF varchar(10) DEFAULT NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'TRANSACAO')
	Alter table dbo.tb_envio Add TRANSACAO datetime NOT NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'TIP_ROMANEIO')
	Alter table dbo.tb_envio Add TIP_ROMANEIO varchar(50) NOT NULL
GO

IF Not EXISTS (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'tb_envio'
               And Column_Name = 'BD_ROMANEIO')
	Alter table dbo.tb_envio Add BD_ROMANEIO varchar(50) NOT NULL
GO