USE [PROD_2023]
GO

/****** Object:  Table [dbo].[ENXES]    Script Date: 28/06/2024 08:51:57 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[ENXES](
	[Num_CgcCpf] [varchar](14) NOT NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Cod_RegTri] [int] NULL,
	[Cod_AgeCob] [int] NULL,
	[Cod_Transp] [int] NULL,
	[Cod_Rota] [int] NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[Cod_EstEnt] [varchar](14) NULL,
	[Cod_EstabeEntida] [varchar](14) NULL,
	[Cod_IdeEntida] [varchar](20) NULL,
	[Des_LayoutPdc] [varchar](25) NULL,
	[Des_DirPedPdc] [varchar](100) NULL,
	[Des_DirPrePdc] [varchar](100) NULL,
	[Des_DirNotPdc] [varchar](100) NULL,
	[Des_PrgComPdc] [varchar](100) NULL,
	[Des_UltArqPdc] [varchar](50) NULL,
	[Dat_UltArqPdc] [smalldatetime] NULL,
	[Num_SeqArqPdc] [int] NULL,
	[Des_EnderecoFtpPdc] [varchar](256) NULL,
	[Des_UsuarioFtpPdc] [varchar](40) NULL,
	[Des_SenhaFtpPdc] [varchar](30) NULL,
	[Vlr_UltimaFatura] [numeric](18, 4) NULL,
	[Data_UltimaFatura] [smalldatetime] NULL,
	[Flg_Padrao] [bit] NULL,
	[Sta_ClaAbcVal] [varchar](1) NULL,
	[Per_ParticFat] [numeric](18, 8) NULL,
	[Flg_NaoContribuinteIcm] [bit] NULL,
	[Per_Frete] [numeric](18, 4) NULL,
	[Vlr_Minimo] [numeric](18, 4) NULL,
	[Per_ComAtoEnt] [numeric](18, 4) NULL,
	[Flg_PriFat] [bit] NULL,
	[Flg_Coleta] [bit] NULL,
	[Flg_CalFreVlrMer] [bit] NULL,
	[Cod_DisTra] [varchar](14) NULL,
	[UF_PlaVei] [varchar](4) NULL,
	[Cod_PlaVei] [varchar](10) NULL,
	[Cod_Client] [int] NOT NULL,
	[Cod_CadFor] [int] NOT NULL,
	[Cod_CadTra] [int] NOT NULL,
	[Flg_DscIcmDesoneNotFis] [bit] NULL,
	[Cod_AgePag] [int] NULL,
	[Cod_Vendedor] [int] NULL,
	[Cod_Operador] [int] NULL,
	[Des_FtpUrlPdc] [varchar](300) NULL,
	[Des_FtpUsrPdc] [varchar](50) NULL,
	[Des_FtpSnhPdc] [varchar](50) NULL,
	[Flg_BlqEmbDscItePrcUni] [bit] NULL,
	[Flg_BlqIncSbtPrc] [bit] NULL,
	[Flg_BlqIncSbtPriDup] [bit] NULL,
	[Flg_BlqFecVlrPdvNfsSbtEmb] [bit] NULL,
	[Flg_ImpSbtEmbPrc] [bit] NULL,
	[Flg_InfXmlSbtEmbPrc] [bit] NULL,
	[Cod_TabPrc] [int] NULL,
	[Cod_TabPrz] [int] NULL,
	[Qtd_PrzMax] [int] NULL,
	[Cod_OpeSai] [varchar](3) NULL,
	[Cod_Comprador] [int] NULL,
 CONSTRAINT [PK_ENXES] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Num_CgcCpf] ASC,
	[Cod_Client] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_RegTri]  DEFAULT ((0)) FOR [Cod_RegTri]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_AgeCob]  DEFAULT ((0)) FOR [Cod_AgeCob]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_Transp]  DEFAULT ((0)) FOR [Cod_Transp]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_Rota]  DEFAULT ((0)) FOR [Cod_Rota]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Num_SeqArqPdc]  DEFAULT ((0)) FOR [Num_SeqArqPdc]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Vlr_UltimaFatura]  DEFAULT ((0)) FOR [Vlr_UltimaFatura]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_Padrao]  DEFAULT ((0)) FOR [Flg_Padrao]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Per_ParticFat]  DEFAULT ((0)) FOR [Per_ParticFat]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_NaoContribuinteIcm]  DEFAULT ((0)) FOR [Flg_NaoContribuinteIcm]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Per_Frete]  DEFAULT ((0)) FOR [Per_Frete]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Vlr_Minimo]  DEFAULT ((0)) FOR [Vlr_Minimo]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Per_ComAtoEnt]  DEFAULT ((0)) FOR [Per_ComAtoEnt]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_PriFat]  DEFAULT ((0)) FOR [Flg_PriFat]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_Coleta]  DEFAULT ((0)) FOR [Flg_Coleta]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_CalFreVlrMer]  DEFAULT ((0)) FOR [Flg_CalFreVlrMer]
GO

ALTER TABLE [dbo].[ENXES] ADD  DEFAULT ((0)) FOR [Cod_Client]
GO

ALTER TABLE [dbo].[ENXES] ADD  DEFAULT ((0)) FOR [Cod_CadFor]
GO

ALTER TABLE [dbo].[ENXES] ADD  DEFAULT ((0)) FOR [Cod_CadTra]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_DscIcmDesoneNotFis]  DEFAULT ((0)) FOR [Flg_DscIcmDesoneNotFis]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_BlqEmbDscItePrcUni]  DEFAULT ((0)) FOR [Flg_BlqEmbDscItePrcUni]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_BlqIncSbtPrc]  DEFAULT ((0)) FOR [Flg_BlqIncSbtPrc]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_BlqIncSbtPriDup]  DEFAULT ((0)) FOR [Flg_BlqIncSbtPriDup]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_BlqFecVlrPdvNfsSbtEmb]  DEFAULT ((0)) FOR [Flg_BlqFecVlrPdvNfsSbtEmb]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_ImpSbtEmbPrc]  DEFAULT ((0)) FOR [Flg_ImpSbtEmbPrc]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Flg_InfXmlSbtEmbPrc]  DEFAULT ((0)) FOR [Flg_InfXmlSbtEmbPrc]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_TabPrc]  DEFAULT ((0)) FOR [Cod_TabPrc]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_TabPrz]  DEFAULT ((0)) FOR [Cod_TabPrz]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Qtd_PrzMax]  DEFAULT ((0)) FOR [Qtd_PrzMax]
GO

ALTER TABLE [dbo].[ENXES] ADD  CONSTRAINT [DF_ENXES_Cod_Comprador]  DEFAULT ((0)) FOR [Cod_Comprador]
GO


