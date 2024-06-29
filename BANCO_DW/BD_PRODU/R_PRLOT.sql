USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_PRLOT]    Script Date: 28/06/2024 13:01:41 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_PRLOT](
	[Cod_Produt] [int] NOT NULL,
	[Cod_Lote] [varchar](20) NOT NULL,
	[Dat_Vencim] [smalldatetime] NULL,
	[Cod_Etique] [int] NULL,
	[Qtd_Fisico] [int] NULL,
	[Qtd_Solicitado] [int] NULL,
	[Qtd_Saldo] [int] NULL,
	[Qtd_FisPra] [int] NULL,
	[Qtd_SolPra] [int] NULL,
	[Qtd_SldPra] [int] NULL,
	[Qtd_FisDep] [int] NULL,
	[Qtd_SolDep] [int] NULL,
	[Qtd_SldDep] [int] NULL,
	[Dat_Fabric] [smalldatetime] NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Cod_Dep] [int] NOT NULL,
	[Dat_Entrada] [smalldatetime] NULL,
	[Fat_CnvAprEmbPad] [int] NOT NULL,
 CONSTRAINT [PK_R_PRLOT] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Cod_Dep] ASC,
	[Cod_Produt] ASC,
	[Cod_Lote] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_Fisico]  DEFAULT (0) FOR [Qtd_Fisico]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_Solicitado]  DEFAULT (0) FOR [Qtd_Solicitado]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_Saldo]  DEFAULT (0) FOR [Qtd_Saldo]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_FisPra]  DEFAULT (0) FOR [Qtd_FisPra]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_SolPra]  DEFAULT (0) FOR [Qtd_SolPra]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_SldPra]  DEFAULT (0) FOR [Qtd_SldPra]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_FisDep]  DEFAULT (0) FOR [Qtd_FisDep]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_SolDep]  DEFAULT (0) FOR [Qtd_SolDep]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Qtd_SldDep]  DEFAULT (0) FOR [Qtd_SldDep]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Cod_Estabe]  DEFAULT ((0)) FOR [Cod_Estabe]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  CONSTRAINT [DF_R_PRLOT_Cod_Dep]  DEFAULT ((0)) FOR [Cod_Dep]
GO

ALTER TABLE [dbo].[R_PRLOT] ADD  DEFAULT ((1)) FOR [Fat_CnvAprEmbPad]
GO

ALTER TABLE [dbo].[R_PRLOT]  WITH NOCHECK ADD  CONSTRAINT [FK_R_PRLOT_PRODU] FOREIGN KEY([Cod_Produt])
REFERENCES [dbo].[R_PRODU] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_PRLOT] CHECK CONSTRAINT [FK_R_PRLOT_PRODU]
GO


