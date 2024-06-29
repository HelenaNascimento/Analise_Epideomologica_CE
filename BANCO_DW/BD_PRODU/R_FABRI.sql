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
	[NovoCodigo] [int] NULL,
	[_Flg_TransmNeo] [bit] NULL,
 CONSTRAINT [PK_R_FABRI] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Cod_ForPref]  DEFAULT (0) FOR [_Cod_ForPref]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_ParticFat]  DEFAULT (0) FOR [Per_ParticFat]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_TransmItx]  DEFAULT (0) FOR [_Flg_TransmItx]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_TransmPro]  DEFAULT (0) FOR [_Flg_TransmPro]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Qtd_PrzMaxFat]  DEFAULT (0) FOR [Qtd_PrzMaxFat]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_DscMaxVis]  DEFAULT (0) FOR [Per_DscMaxVis]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_DscMaxPrz]  DEFAULT (0) FOR [Per_DscMaxPrz]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_Desconto]  DEFAULT (0) FOR [Flg_Desconto]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_BlqInfPar]  DEFAULT (0) FOR [Flg_BlqInfPar]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_DscBasComNor]  DEFAULT (0) FOR [Per_DscBasComNor]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_DscBasTax]  DEFAULT (0) FOR [Per_DscBasTax]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_ComBasTax]  DEFAULT (0) FOR [Per_ComBasTax]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_BlqIms]  DEFAULT (0) FOR [_Flg_BlqIms]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_Exclusivi]  DEFAULT (0) FOR [Flg_Exclusivi]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_InfCnvNfs]  DEFAULT ((0)) FOR [Flg_InfCnvNfs]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_TransmGnx]  DEFAULT ((0)) FOR [_Flg_TransmGnx]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Bloqueado]  DEFAULT ((0)) FOR [Bloqueado]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Per_MarkupCusCom]  DEFAULT ((0)) FOR [Per_MarkupCusCom]
GO

ALTER TABLE [dbo].[R_FABRI] ADD  CONSTRAINT [DF_R_FABRI_Flg_TransmNeo]  DEFAULT ((0)) FOR [_Flg_TransmNeo]
GO


