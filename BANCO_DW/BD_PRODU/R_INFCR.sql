USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_INFCR]    Script Date: 28/06/2024 10:26:02 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_INFCR](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](30) NULL,
	[Controle] [char](1) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL)
