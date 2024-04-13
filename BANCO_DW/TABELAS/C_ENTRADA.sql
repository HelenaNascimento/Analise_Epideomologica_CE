USE [DW_PROD]
GO

/****** Object:  Table [dbo].[C_ENTRADA]    Script Date: 13/04/2024 17:05:08 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[C_ENTRADA](
	[Cod_Estada] [int] NULL,
	[Cod_Fabr] [int] NULL,
	[Cod_Prod] [int] NULL,
	[PrcFab] [numeric](20, 4) NULL,
	[PrcUltEnt] [numeric](20, 4) NULL,
	[PrcUnit] [numeric](20, 4) NULL,
	[DatComp] [smalldatetime] NULL
) ON [PRIMARY]
GO


