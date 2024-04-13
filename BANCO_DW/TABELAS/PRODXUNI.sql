USE [DW_PROD]
GO

/****** Object:  Table [dbo].[PRODXUNI]    Script Date: 13/04/2024 17:27:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[PRODXUNI](
	[ANT_EAN] [varchar](14) NULL,
	[POS_EAN] [varchar](14) NULL,
	[ANT_CODIGO] [int] NULL,
	[POS_CODIGO] [int] NULL,
	[ANT_DESC] [varchar](50) NULL,
	[POS_DESC] [varchar](50) NULL,
	[DATA_INICIAL] [varchar](11) NULL
) ON [PRIMARY]
GO


