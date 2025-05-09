USE [BD_DW]
GO

/****** Object:  Table [dbo].[R_BAIRR]    Script Date: 28/06/2024 10:28:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_BAIRR](
	[Cod_Estado] [char](2) NOT NULL,
	[Cod_Cidade] [int] NOT NULL,
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](20) NULL,
	[Cod_MacroReg] [int] NULL,
	[Cod_MicroReg] [int] NULL,
	[Flg_Excluido] [bit] NULL)