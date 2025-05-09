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
	[Fat_CnvAprEmbPad] [int] NOT NULL)