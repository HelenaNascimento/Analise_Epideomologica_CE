USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_ESTAD]    Script Date: 28/06/2024 10:30:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_ESTAD](
	[Codigo] [char](2) NOT NULL,
	[Descricao] [varchar](20) NULL,
	[_Credenciamento] [varchar](15) NULL,
	[_Flg_RegSubsTrib] [bit] NULL,
	[Per_DscTri] [numeric](18, 8) NULL,
	[ObservacaoNF] [varchar](80) NULL,
	[Obs2_NF] [varchar](80) NULL,
	[Obs3_NF] [varchar](80) NULL,
	[Cod_Ibge] [char](2) NULL)

