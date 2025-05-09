USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_AGCOB]    Script Date: 28/06/2024 10:27:20 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_AGCOB](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](25) NULL,
	[Tipo] [varchar](2) NULL,
	[Cod_TipCobDef] [varchar](2) NULL,
	[Cod_Carteira] [int] NULL,
	[Qtd_DiaAtrPer] [int] NULL,
	[Cod_Banco] [int] NULL,
	[Cod_Agencia] [varchar](6) NULL,
	[Num_Conta] [varchar](13) NULL
)
