USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_FABRI]    Script Date: 28/06/2024 08:55:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_FABRI](
	[Codigo] [int] NOT NULL,
	[Fantasia] [varchar](25) NULL,
	[_Cod_ForPref] [int] NULL,
	[Cgc_Cpf] [varchar](14) NULL,
	[Sta_ClaAbcVal] [char](1) NULL,
	[Per_ParticFat] [numeric](18, 8) NULL,
	[_Flg_TransmItx] [bit] NULL,
	[_Flg_TransmPro] [bit] NULL,
	[Email] [varchar](45) NULL,
	[Ord_Bloco] [char](1) NULL,
	[Des_Bloco] [varchar](25) NULL,
	[Qtd_PrzMaxFat] [int] NULL,
	[Per_DscMaxVis] [numeric](18, 8) NULL,
	[Per_DscMaxPrz] [numeric](18, 8) NULL,
	[Flg_Desconto] [bit] NULL,
	[Flg_BlqInfPar] [bit] NULL,
	[Per_DscBasComNor] [numeric](18, 8) NULL,
	[Per_DscBasTax] [numeric](18, 8) NULL,
	[Per_ComBasTax] [numeric](18, 8) NULL,
	[_Cod_DisFab] [varchar](14) NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[_Flg_BlqIms] [bit] NULL,
	[Flg_Exclusivi] [bit] NULL,
	[Cod_Estado] [char](2) NULL,
	[Flg_InfCnvNfs] [bit] NULL,
	[_Flg_TransmGnx] [bit] NULL,
	[_Cod_FabGnx] [varchar](6) NULL,
	[Bloqueado] [bit] NULL,
	[Per_MarkupCusCom] [numeric](18, 4) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL)