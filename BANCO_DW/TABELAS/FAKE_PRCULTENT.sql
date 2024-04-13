USE [DW_PROD]
GO

/****** Object:  Table [dbo].[FAKE_PRCULTENT]    Script Date: 13/04/2024 17:21:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[FAKE_PRCULTENT](
	[CodProd] [int] NULL,
	[Prc_UltEnt] [numeric](20, 2) NULL,
	[Mes] [int] NULL,
	[Ano] [int] NULL,
	[Dt_Alt] [smalldatetime] NULL
) ON [PRIMARY]
GO


