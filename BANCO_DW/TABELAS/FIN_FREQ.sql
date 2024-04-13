USE [DW_PROD]
GO

/****** Object:  Table [dbo].[FIN_FREQ]    Script Date: 13/04/2024 17:21:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[FIN_FREQ](
	[Frequencia] [varchar](20) NULL,
	[D_MIN] [int] NULL,
	[D_MAX] [int] NULL,
	[Qtd_Bol] [int] NULL
) ON [PRIMARY]
GO


