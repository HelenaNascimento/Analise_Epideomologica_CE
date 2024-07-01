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
	[Cod_EnqIpi] [varchar](3) NULL,
 CONSTRAINT [PK_R_GRCLI] PRIMARY KEY CLUSTERED 
(
	[Cod_GrpCli] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Qtd_PrzMax]  DEFAULT (0) FOR [Qtd_PrzMax]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Per_Descon]  DEFAULT (0) FOR [Per_Descon]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Per_DscVis]  DEFAULT (0) FOR [Per_DscVis]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Flg_ComFab]  DEFAULT (0) FOR [Flg_ComFab]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Flg_ComPrpDsc]  DEFAULT (0) FOR [Flg_ComPrpDsc]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Per_ComVnd]  DEFAULT (0) FOR [Per_ComVnd]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Flg_ExpBomPrc]  DEFAULT ((0)) FOR [Flg_ExpBomPrc]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Per_RntMinPdv]  DEFAULT ((0)) FOR [Per_RntMinPdv]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Flg_LotUniPrdPdv]  DEFAULT ((0)) FOR [Flg_LotUniPrdPdv]
GO

ALTER TABLE [dbo].[R_GRCLI] ADD  CONSTRAINT [DF_GRCLI_Versao]  DEFAULT ((0)) FOR [Versao]
GO


