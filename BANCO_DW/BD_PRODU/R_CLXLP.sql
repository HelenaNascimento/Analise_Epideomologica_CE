USE [PROD_2023]
GO

/****** Object:  Table [dbo].[CLXLP]    Script Date: 28/06/2024 09:36:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[CLXLP](
	[Cod_Client] [int] NOT NULL,
	[Cod_Layout] [int] NOT NULL,
	[Cod_Controle] [varchar](13) NULL,
	[Des_DirRet] [varchar](100) NULL)


