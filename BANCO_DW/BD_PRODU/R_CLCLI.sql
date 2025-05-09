USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_CLCLI]    Script Date: 28/06/2024 10:29:16 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_CLCLI](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](50) NOT NULL,
	[Des_Cor] [varchar](15) NOT NULL,
	[Vlr_FaiIni] [numeric](18, 4) NOT NULL,
	[Vlr_FaiFin] [numeric](18, 4) NOT NULL)

