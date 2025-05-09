USE BD_DW
GO

/****** Object:  Table [dbo].[GRCLI]    Script Date: 01/07/2024 16:57:57 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_GRCLI](
	[Cod_GrpCli] [int] NOT NULL,
	[Des_GrpCli] [varchar](25) NULL,
	[Qtd_PrzMax] [int] NULL,
	[Per_Descon] [numeric](18, 8) NULL,
	[Per_DscVis] [numeric](18, 8) NULL,
	[Flg_ComFab] [bit] NULL,
	[Flg_ComPrpDsc] [bit] NULL,
	[Usuario] [varchar](15) NULL,
	[Transacao] [smalldatetime] NULL,
	[Per_ComVnd] [numeric](18, 4) NULL,
	[Flg_ExpBomPrc] [bit] NULL,
	[Des_UrlExpBomPrc] [varchar](255) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Per_RntMinPdv] [numeric](18, 4) NULL,
	[Flg_LotUniPrdPdv] [bit] NULL,
	[Versao] [bigint] NOT NULL,
	[Cod_EnqIpi] [varchar](3) NULL)