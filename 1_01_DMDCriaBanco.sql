/***** CRIA O BANCO "DMD", E O LOGIN "DMDAPP" *****/

USE MASTER
GO

CREATE DATABASE [DMD]ON --Colocar Unidade onde ficarão os arquivos físicos do Banco de dados
  (NAME = N'DMD_DATA', 
   FILENAME = N'C:\Infarma\DMD\DMD.mdf' , SIZE = 10, FILEGROWTH = 50%) 
LOG ON 
  (NAME = N'DMD_LOG', FILENAME = N'C:\Infarma\DMD\DMD.ldf' , SIZE = 10, FILEGROWTH = 20%)
 COLLATE SQL_Latin1_General_CP1_CI_AS
GO

ALTER DATABASE DMD SET AUTO_CLOSE ON
--exec sp_dboption N'DMD', N'autoclose', N'true'
GO

ALTER DATABASE DMD SET RECOVERY FULL -- SE ('bulkcopy'=false) E ('trunc. log'=false)
--exec sp_dboption N'DMD', N'bulkcopy', N'false'
GO

--ALTER DATABASE DMD SET RECOVERY FULL (Leia comentário acima)
--exec sp_dboption N'DMD', N'trunc. log', N'false'
--GO

ALTER DATABASE DMD SET TORN_PAGE_DETECTION ON 
--exec sp_dboption N'DMD', N'torn page detection', N'true'
GO

ALTER DATABASE DMD SET READ_WRITE
--exec sp_dboption N'DMD', N'read only', N'false'
GO

ALTER DATABASE DMD SET MULTI_USER 
--exec sp_dboption N'DMD', N'dbo use', N'false'
GO

--ALTER DATABASE DMD SET MULTI_USER (é o mesmo comando que o de cima agora)
--exec sp_dboption N'DMD', N'single', N'false'
--GO

ALTER DATABASE DMD SET AUTO_SHRINK OFF
--exec sp_dboption N'DMD', N'autoshrink', N'false'
GO

ALTER DATABASE DMD SET ANSI_NULL_DEFAULT OFF
--exec sp_dboption N'DMD', N'ANSI null default', N'false'
GO

ALTER DATABASE DMD SET RECURSIVE_TRIGGERS OFF
--exec sp_dboption N'DMD', N'recursive triggers', N'false'
GO

ALTER DATABASE DMD SET ANSI_NULL_DEFAULT OFF
--exec sp_dboption N'DMD', N'ANSI nulls', N'false'
GO

ALTER DATABASE DMD SET CONCAT_NULL_YIELDS_NULL OFF
--exec sp_dboption N'DMD', N'concat null yields null', N'false'
GO

ALTER DATABASE DMD SET CURSOR_CLOSE_ON_COMMIT ON 
--exec sp_dboption N'DMD', N'cursor close on commit', N'true'
GO

ALTER DATABASE DMD SET CURSOR_DEFAULT GLOBAL
--exec sp_dboption N'DMD', N'default to local cursor', N'false'
GO

ALTER DATABASE DMD SET QUOTED_IDENTIFIER OFF
--exec sp_dboption N'DMD', N'quoted identifier', N'false'
GO

ALTER DATABASE DMD SET ANSI_WARNINGS OFF
--exec sp_dboption N'DMD', N'ANSI warnings', N'false'
GO

ALTER DATABASE DMD SET AUTO_CREATE_STATISTICS ON 
--exec sp_dboption N'DMD', N'auto create statistics', N'true'
GO

ALTER DATABASE DMD SET AUTO_UPDATE_STATISTICS ON 
--exec sp_dboption N'DMD', N'auto update statistics', N'true'
GO

use [DMD]
GO

if not exists (select * from master..syslogins where name = N'DMDApp')
	EXEC sp_addlogin N'DMDApp', N'DMD20051643', N'DMD', N'Português'
GO

exec sp_addsrvrolemember N'DMDApp', sysadmin
GO

if not exists (select * from sysusers where name = N'DMDApp' and uid < 16382)
	EXEC sp_grantdbaccess N'DMDApp', N'DMDApp'
GO
