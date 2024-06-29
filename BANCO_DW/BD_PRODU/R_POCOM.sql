USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_POCOM]    Script Date: 28/06/2024 10:57:30 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_POCOM](
	[Id_PolCom] [int] NOT NULL,
	[Cod_PolCom] [varchar](25) NOT NULL,
	[Des_Detalhada] [varchar](120) NULL,
	[Dat_Inicio] [smalldatetime] NULL,
	[Dat_Termino] [smalldatetime] NULL,
	[Bloqueado] [bit] NULL,
	[Qtd_Minimo] [smallint] NULL,
	[Vlr_Minimo] [numeric](18, 4) NULL,
	[Per_Desconto] [numeric](18, 4) NULL,
	[Qtd_PrzMax] [smallint] NULL,
	[Cod_TipPrz] [char](1) NULL,
	[Cod_TabPrc] [int] NULL,
	[Cod_TabPrz] [int] NULL,
	[Cod_TabComVdr] [int] NULL,
	[Cod_TabComOpe] [int] NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[Flg_Televendas] [bit] NULL,
	[Flg_Cfv] [bit] NULL,
	[Flg_PedEle] [bit] NULL,
	[Flg_Web] [bit] NULL,
	[Qtd_IteMin] [int] NULL,
	[Per_DscCom] [numeric](18, 4) NULL,
	[Per_DscFin] [numeric](18, 4) NULL,
	[Per_RedComVdr] [numeric](18, 4) NULL,
	[Per_RedComOpe] [numeric](18, 4) NULL,
	[Flg_NegDsc] [bit] NULL,
	[Flg_BlqCli] [bit] NULL,
	[Flg_Balcao] [bit] NULL,
	[Flg_ExcCla] [bit] NULL,
	[Flg_ExcFab] [bit] NULL,
	[Flg_NaoSugDscItePdv] [bit] NULL,
	[Dat_Cadastro] [smalldatetime] NULL,
	[Per_DscAutPrc] [numeric](18, 4) NULL,
	[Per_AcrAutPrc] [numeric](18, 4) NULL,
	[Flg_PrcVenPrpPrz] [bit] NULL,
	[Per_CorPrcVenPrpPrz] [numeric](18, 4) NULL,
	[Flg_BlqVerba] [bit] NULL,
	[Flg_BlqPolDifItePdv] [bit] NULL,
	[Flg_BlqPolDifCabPdv] [bit] NULL,
	[Flg_MarkupCusCom] [bit] NULL,
	[Tip_RatBon] [varchar](1) NULL,
	[Per_DscComVis] [numeric](18, 4) NULL,
	[Per_DscFinVis] [numeric](18, 4) NULL,
	[Flg_SugDscGlbPdv] [bit] NULL,
	[Tip_PolCom] [varchar](4) NULL,
	[Flg_DbqUsoVrbPdv] [bit] NULL,
	[Flg_GerVrbPdv] [bit] NULL,
	[Tip_VrbPdv] [varchar](1) NULL,
	[Flg_SugPrzCli] [bit] NULL,
	[Controle_Saldo] [varchar](1) NULL,
	[Flg_AcuVrbPos] [bit] NULL,
	[Flg_AcuVrbNeg] [bit] NULL,
	[Flg_CtrPdvVrbVdr] [bit] NULL,
	[Flg_CtrPdvVrbOpe] [bit] NULL,
	[Per_ComAtvVdr] [numeric](18, 4) NULL,
	[Per_ComPasVdr] [numeric](18, 4) NULL,
	[Per_ComAtvOpe] [numeric](18, 4) NULL,
	[Per_ComPasOpe] [numeric](18, 4) NULL,
	[Obs_PolCom] [text] NULL,
	[Flg_UsaTabPrcCadCli] [bit] NULL,
	[Dat_Criacao] [datetime] NULL,
	[Cod_TipComiss] [varchar](1) NULL,
	[Per_ComVdr] [numeric](7, 4) NULL,
	[Per_ComOpe] [numeric](7, 4) NULL,
	[Flg_BlqCriPrcMin] [bit] NULL,
	[Flg_CriPrcMin] [bit] NULL,
 CONSTRAINT [PK_R_POCOM] PRIMARY KEY CLUSTERED 
(
	[Id_PolCom] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Bloqueado]  DEFAULT (0) FOR [Bloqueado]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Qtd_Minimo]  DEFAULT (0) FOR [Qtd_Minimo]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Vlr_Minimo]  DEFAULT (0) FOR [Vlr_Minimo]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_Desconto]  DEFAULT (0) FOR [Per_Desconto]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_Televendas]  DEFAULT (0) FOR [Flg_Televendas]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_Cfv]  DEFAULT (0) FOR [Flg_Cfv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_PedEle]  DEFAULT (0) FOR [Flg_PedEle]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_Web]  DEFAULT (0) FOR [Flg_Web]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Qtd_IteMin]  DEFAULT (0) FOR [Qtd_IteMin]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_DscCom]  DEFAULT (0) FOR [Per_DscCom]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_DscFin]  DEFAULT (0) FOR [Per_DscFin]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_RedComVdr]  DEFAULT (0) FOR [Per_RedComVdr]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_RedComOpe]  DEFAULT (0) FOR [Per_RedComOpe]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_NegDsc]  DEFAULT (0) FOR [Flg_NegDsc]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_BlqCli]  DEFAULT (0) FOR [Flg_BlqCli]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_Balcao]  DEFAULT (0) FOR [Flg_Balcao]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_ExcCla]  DEFAULT (0) FOR [Flg_ExcCla]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_ExcFab]  DEFAULT (0) FOR [Flg_ExcFab]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_NaoSugDscItePdv]  DEFAULT (0) FOR [Flg_NaoSugDscItePdv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_DscAutPrc]  DEFAULT ((0)) FOR [Per_DscAutPrc]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_AcrAutPrc]  DEFAULT ((0)) FOR [Per_AcrAutPrc]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_PrcVenPrpPrz]  DEFAULT ((0)) FOR [Flg_PrcVenPrpPrz]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_CorPrcVenPrpPrz]  DEFAULT ((0)) FOR [Per_CorPrcVenPrpPrz]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_BlqVerba]  DEFAULT ((0)) FOR [Flg_BlqVerba]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_BlqPolDifItePdv]  DEFAULT ((0)) FOR [Flg_BlqPolDifItePdv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_BlqPolDifCabPdv]  DEFAULT ((0)) FOR [Flg_BlqPolDifCabPdv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_MarkupCusCom]  DEFAULT ((0)) FOR [Flg_MarkupCusCom]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_DscComVis]  DEFAULT ((0)) FOR [Per_DscComVis]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Per_DscFinVis]  DEFAULT ((0)) FOR [Per_DscFinVis]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_SugDscGlbPdv]  DEFAULT ((0)) FOR [Flg_SugDscGlbPdv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_DbqUsoVrbPdv]  DEFAULT ((0)) FOR [Flg_DbqUsoVrbPdv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  CONSTRAINT [DF_R_POCOM_Flg_GerVrbPdv]  DEFAULT ((0)) FOR [Flg_GerVrbPdv]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  DEFAULT ((0)) FOR [Flg_SugPrzCli]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  DEFAULT ((0)) FOR [Flg_UsaTabPrcCadCli]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  DEFAULT ((0)) FOR [Per_ComVdr]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  DEFAULT ((0)) FOR [Per_ComOpe]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  DEFAULT ((1)) FOR [Flg_BlqCriPrcMin]
GO

ALTER TABLE [dbo].[R_POCOM] ADD  DEFAULT ((1)) FOR [Flg_CriPrcMin]
GO


