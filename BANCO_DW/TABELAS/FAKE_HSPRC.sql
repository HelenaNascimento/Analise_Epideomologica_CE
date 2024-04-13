USE [DW_PROD]
GO

/****** Object:  Table [dbo].[FAKE_HSPRC]    Script Date: 13/04/2024 17:19:45 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[FAKE_HSPRC](
	[CodProd] [int] NULL,
	[Prc_Pr] [numeric](20, 2) NULL,
	[CM] [numeric](20, 2) NULL,
	[Ano] [int] NULL,
	[Mes] [int] NULL,
	[Dt_Alt] [smalldatetime] NULL
) ON [PRIMARY]
GO


