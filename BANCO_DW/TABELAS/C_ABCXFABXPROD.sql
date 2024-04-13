USE [DW_PROD]
GO

/****** Object:  Table [dbo].[C_ABCXFABXPROD]    Script Date: 13/04/2024 17:04:03 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[C_ABCXFABXPROD](
	[Cod_Fabri] [int] NULL,
	[Cod_Ean] [varchar](14) NULL,
	[Codigo] [int] NULL,
	[Descri] [varchar](80) NULL,
	[Cod_PolCom] [varchar](50) NULL,
	[Auxilixar] [varchar](80) NULL,
	[Qtd_Vend] [int] NULL,
	[VlrFatVen] [decimal](20, 4) NULL,
	[VlrBasDscVen] [decimal](20, 4) NULL,
	[Mes_Fat] [varchar](4) NULL
) ON [PRIMARY]
GO


