USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_RMATV]    Script Date: 28/06/2024 10:23:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_RMATV](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](30) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Flg_RedAlqVenPisCof] [bit] NULL,
	[Flg_BlqCtrLicSauVen] [bit] NULL,
	[Flg_BlqPdvLicAnvVen] [bit] NULL,
	[Flg_BlqPdvLicCrfVen] [bit] NULL,
	[Flg_BlqPdvAlvFunVen] [bit] NULL,
	[Flg_BlqCriLicCot] [bit] NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Flg_BlqPdvLicVen] [bit] NULL)