USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_NFSIT]    Script Date: 28/06/2024 13:05:32 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_NFSIT](
	[Cod_Estabe] [int] NOT NULL,
	[Ser_Nota] [varchar](3) NOT NULL,
	[Num_Nota] [int] NOT NULL,
	[Cod_Produto] [int] NOT NULL,
	[Cod_Lote] [varchar](20) NOT NULL,
	[Num_SeqIte] [int] NOT NULL,
	[Qtd_Produto] [int] NULL,
	[Qtd_Bonificacao] [int] NULL,
	[Flg_DscItePdv] [bit] NULL,
	[Per_Desconto] [numeric](18, 8) NULL,
	[Prc_Unitario] [numeric](18, 8) NULL,
	[Vlr_CusIte] [numeric](18, 4) NULL,
	[Cod_Promocao] [int] NULL,
	[Tip_Aux] [char](1) NULL,
	[Cod_ClaTri] [varchar](4) NULL,
	[Ctrl_Tributacao] [char](2) NULL,
	[Alq_Icms] [numeric](18, 8) NULL,
	[Vlr_IcmsNor] [numeric](18, 4) NULL,
	[Vlr_RepIcms] [numeric](18, 4) NULL,
	[Vlr_SubsTrib] [numeric](18, 4) NULL,
	[Vlr_IcmsTri] [numeric](18, 4) NULL,
	[Vlr_BasIcmsNor] [numeric](18, 4) NULL,
	[Vlr_BasRepIcms] [numeric](18, 4) NULL,
	[Vlr_PrdSubTri] [numeric](18, 4) NULL,
	[Vlr_BasSubsTrib] [numeric](18, 4) NULL,
	[Vlr_BasIcmsTri] [numeric](18, 4) NULL,
	[Vlr_Isento] [numeric](18, 4) NULL,
	[Vlr_Ipi] [numeric](18, 4) NULL,
	[Vlr_DscTri] [numeric](18, 4) NULL,
	[Vlr_TotItem] [numeric](18, 4) NULL,
	[Vlr_LiqItem] [numeric](18, 4) NULL,
	[Vlr_BruItem] [numeric](18, 4) NULL,
	[Flg_BlqDsc] [bit] NULL,
	[Per_Descon] [numeric](18, 8) NULL,
	[Per_Comissao] [numeric](18, 8) NULL,
	[Vlr_Comissao] [numeric](18, 4) NULL,
	[Per_ComTlmkt] [numeric](18, 8) NULL,
	[Vlr_ComTlmkt] [numeric](18, 4) NULL,
	[Flg_ComisNormal] [bit] NULL,
	[Prc_Tabela] [numeric](18, 4) NULL,
	[Cod_Cfo] [smallint] NULL,
	[Vlr_Outros] [numeric](18, 4) NULL,
	[Vlr_DespRateada] [numeric](18, 4) NULL,
	[Vlr_DescRateado] [numeric](18, 4) NULL,
	[Prc_TabBru] [numeric](18, 4) NULL,
	[Qtd_ImpFat] [numeric](18, 4) NULL,
	[Prc_UniImpFat] [numeric](18, 8) NULL,
	[Qtd_Pra] [int] NULL,
	[Qtd_Dep] [int] NULL,
	[Prc_MaxCon] [numeric](18, 4) NULL,
	[Prc_Fabric] [numeric](18, 4) NULL,
	[Per_ComTra] [numeric](18, 4) NULL,
	[Val_ComTra] [numeric](18, 4) NULL,
	[Des_UnvImpFat] [char](3) NULL,
	[Des_AprImpFat] [varchar](12) NULL,
	[Flg_BlqInfPar] [bit] NULL,
	[Transacao] [smalldatetime] NULL,
	[Fat_CnvImpFat] [int] NULL,
	[Per_DescontoFin] [numeric](18, 4) NULL,
	[Flg_IncFis] [bit] NULL,
	[Cod_ModBasCalIcm] [char](1) NULL,
	[Cod_ModBasCalIcmSbt] [char](1) NULL,
	[Per_RedBasCalIcm] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmSbt] [numeric](18, 4) NULL,
	[Alq_IcmSbt] [numeric](18, 4) NULL,
	[Vlr_BasTri] [numeric](18, 8) NULL,
	[Alq_AgrSbt] [numeric](18, 4) NULL,
	[Vlr_Frete] [numeric](18, 4) NULL,
	[Vlr_Seguro] [numeric](18, 4) NULL,
	[Vlr_BasPis] [numeric](18, 4) NULL,
	[Alq_Pis] [numeric](18, 4) NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[Vlr_BasCofins] [numeric](18, 4) NULL,
	[Alq_Cofins] [numeric](18, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[CST_Pis] [varchar](2) NULL,
	[CST_Cofins] [varchar](2) NULL,
	[Vlr_BasRecSbt] [numeric](18, 4) NULL,
	[Vlr_RecSbt] [numeric](18, 4) NULL,
	[Vlr_BasIpi] [numeric](18, 4) NULL,
	[Alq_Ipi] [numeric](18, 4) NULL,
	[CST_Ipi] [varchar](2) NULL,
	[Vlr_SubsTribEmb] [numeric](18, 4) NULL,
	[Id_PolCom] [int] NULL,
	[Vlr_Verba] [numeric](18, 4) NULL,
	[Vlr_PrcBasRecSbt] [numeric](18, 8) NULL,
	[Alq_AgrDebRecSbt] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmDebRecSbt] [numeric](18, 4) NULL,
	[Alq_IcmDebRecSbt] [numeric](18, 4) NULL,
	[Vlr_BasRecSbtInt] [numeric](18, 8) NULL,
	[Vlr_RecSbtInt] [numeric](18, 8) NULL,
	[Tip_EntSai] [varchar](1) NULL,
	[Cod_GrpPrc] [varchar](1) NULL,
	[Tip_ConReg] [varchar](1) NULL,
	[Tip_Receit] [varchar](1) NULL,
	[Cod_Tribut] [varchar](3) NULL,
	[Nom_Compra] [varchar](35) NULL,
	[Num_DocCom] [varchar](15) NULL,
	[Des_OrgEmi] [varchar](10) NULL,
	[Des_EstEmi] [varchar](2) NULL,
	[Est_ConReg] [varchar](2) NULL,
	[Val_Descon] [numeric](18, 4) NULL,
	[Per_DscPol] [numeric](18, 4) NULL,
	[Val_DscPol] [numeric](18, 4) NULL,
	[Per_DscPrm] [numeric](18, 4) NULL,
	[Val_DscPrm] [numeric](18, 4) NULL,
	[Val_CusTab] [numeric](18, 4) NULL,
	[Val_CusMed] [numeric](18, 4) NULL,
	[Dat_Receit] [smalldatetime] NULL,
	[Num_Nra] [int] NULL,
	[Qtd_PreNra] [int] NULL,
	[Num_ConReg] [int] NULL,
	[Flg_Cancel] [bit] NULL,
	[Flg_Prescr] [bit] NULL,
	[Prc_LiqUltEnt] [numeric](18, 8) NULL,
	[Vlr_DescItem] [numeric](18, 4) NULL,
	[Vlr_LiqVenIte] [numeric](18, 4) NULL,
	[Vlr_BasSbtRes] [numeric](18, 4) NULL,
	[Vlr_SbtRes] [numeric](18, 4) NULL,
	[Per_ComGer] [numeric](18, 4) NULL,
	[Vlr_ComGer] [numeric](18, 4) NULL,
	[Per_ComSup] [numeric](18, 4) NULL,
	[Vlr_ComSup] [numeric](18, 4) NULL,
	[Per_ComGerOpe] [numeric](18, 4) NULL,
	[Vlr_ComGerOpe] [numeric](18, 4) NULL,
	[Per_ComSupOpe] [numeric](18, 4) NULL,
	[Vlr_ComSupOpe] [numeric](18, 4) NULL,
	[Per_VrbPar] [numeric](18, 4) NULL,
	[Vlr_VrbPar] [numeric](18, 4) NULL,
	[Per_VrbBon] [numeric](18, 4) NULL,
	[Vlr_VrbBon] [numeric](18, 4) NULL,
	[Prc_UniPde] [numeric](18, 4) NULL,
	[Per_DscPde] [numeric](18, 4) NULL,
	[Flg_UsaCreIcmSbt] [bit] NULL,
	[Prc_RefFpb] [numeric](18, 4) NULL,
	[Cod_CSOSN] [varchar](3) NULL,
	[Flg_PolPrm] [bit] NULL,
	[Cod_PrjPde] [varchar](12) NULL,
	[Num_SeqPdc] [varchar](6) NULL,
	[Vlr_DscBonRat] [numeric](18, 4) NULL,
	[Prc_CusMinComMarkup] [numeric](18, 4) NULL,
	[Vlr_VrbVdr] [numeric](18, 4) NULL,
	[Vlr_VrbSup] [numeric](18, 4) NULL,
	[Prc_UniPdv] [numeric](18, 4) NULL,
	[Per_DscPdv] [numeric](18, 4) NULL,
	[Tip_Sai] [varchar](1) NULL,
	[Vlr_ResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_ResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Tip_PrcBasDebSbt] [varchar](1) NULL,
	[Tip_PrcBasCreSbt] [varchar](1) NULL,
	[Vlr_BasResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Alq_ResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_BasResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Alq_ResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Alq_MvaResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_CusComIte] [numeric](18, 4) NULL,
	[Per_AcrAlqIntFcp] [numeric](18, 4) NULL,
	[Per_PrvParIcm] [numeric](18, 4) NULL,
	[Vlr_IcmFcpDes] [numeric](18, 4) NULL,
	[Vlr_IcmParDes] [numeric](18, 4) NULL,
	[Vlr_IcmParRem] [numeric](18, 4) NULL,
	[Num_NfePrcBasRecSbt] [int] NULL,
	[Per_ResExt] [numeric](18, 4) NULL,
	[Vlr_BasDspExt] [numeric](18, 4) NULL,
	[Vlr_DspExt] [numeric](18, 4) NULL,
	[Cod_CEST] [varchar](7) NULL,
	[Cod_Ncm] [varchar](8) NULL,
	[Alq_IntIcmDes] [numeric](18, 4) NULL,
	[Cod_KitPrm] [int] NULL,
	[Cod_TotPar] [varchar](7) NULL,
	[Str_VolEmb] [varchar](50) NULL,
	[Vlr_BasCalSubTriEntMed] [numeric](18, 4) NULL,
	[Alq_IcmPresum] [numeric](18, 4) NULL,
	[Flg_UsaDebPresum] [bit] NULL,
	[Obs_NF] [varchar](200) NULL,
	[Vlr_DscBonDup] [numeric](18, 4) NULL,
	[Vlr_BasDscTri] [numeric](18, 4) NULL,
	[Per_DscTri] [numeric](9, 4) NULL,
	[Vlr_BasVerba] [numeric](18, 4) NULL,
	[Prc_PolCom] [numeric](18, 8) NULL,
	[Per_DscPolCom] [numeric](18, 4) NULL,
	[Alq_IcmDif] [numeric](9, 4) NULL,
	[Vlr_IcmsDif] [numeric](18, 4) NULL,
	[Qtd_PesVolImpFat] [numeric](18, 4) NOT NULL,
	[Des_UnvPesVolImpFat] [varchar](3) NULL,
	[Prc_UniPesVolImpFat] [numeric](18, 8) NOT NULL,
	[Vlr_IcmsDeson] [numeric](18, 4) NULL,
	[Cod_MtvIcmsDeson] [varchar](2) NULL,
	[Vlr_DscCalSuframa] [numeric](18, 4) NULL,
	[Alq_FcpIcm] [numeric](18, 4) NULL,
	[Vlr_FcpIcm] [numeric](18, 4) NULL,
	[Alq_FcpSbt] [numeric](18, 4) NULL,
	[Vlr_FcpSbt] [numeric](18, 4) NULL,
	[Alq_FcpSbtRet] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRet] [numeric](18, 4) NULL,
	[Alq_FcpSbtRec] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRec] [numeric](18, 4) NULL,
	[vBCSTRet] [numeric](18, 4) NULL,
	[pST] [numeric](8, 4) NULL,
	[vICMSSubstituto] [numeric](18, 4) NULL,
	[vICMSSTRet] [numeric](18, 4) NULL,
	[vBCFCPSTRet] [numeric](18, 4) NULL,
	[pFCPSTRet] [numeric](8, 4) NULL,
	[vFCPSTRet] [numeric](18, 4) NULL,
	[pRedBCEfet] [numeric](8, 4) NULL,
	[vBCEfet] [numeric](18, 4) NULL,
	[pICMSEfet] [numeric](8, 4) NULL,
	[vICMSEfet] [numeric](18, 4) NULL,
	[Per_IcmDiferi] [numeric](8, 4) NULL,
	[Vlr_IcmDiferi] [numeric](18, 4) NULL,
	[Vlr_DspDevST] [numeric](18, 4) NULL,
	[cBenef] [varchar](10) NULL,
	[Vlr_PisEstDev] [numeric](18, 4) NULL,
	[Vlr_CofEstDev] [numeric](18, 4) NULL,
	[Per_LucLiq] [numeric](18, 4) NULL,
	[Per_LucBru] [numeric](18, 4) NULL,
	[Vlr_VrbOpe] [numeric](18, 4) NULL,
	[Alq_EfeDas] [numeric](7, 4) NULL,
	[Cod_CtlSldVrb] [varchar](1) NULL,
	[Vlr_PrcBasDevRecSbt] [numeric](18, 4) NULL,
	[Alq_AgrDebDevRecSbt] [numeric](7, 4) NULL,
	[Per_RedBasCalIcmDebDevRecSbt] [numeric](7, 4) NULL,
	[Alq_IcmDebDevRecSbt] [numeric](7, 4) NULL,
	[Vlr_BasDevRecSbt] [numeric](18, 4) NULL,
	[Vlr_DevRecSbt] [numeric](18, 4) NULL,
	[Prc_PonFin] [numeric](18, 4) NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Cod_OriMer] [varchar](3) NULL,
	[Cod_LotPdv] [varchar](20) NULL,
	[Qtd_PesLiq] [numeric](18, 4) NULL,
	[Prc_UniSemAcrIcm] [numeric](18, 8) NULL,
	[Prc_UniComAcrIcm] [numeric](18, 8) NULL,
	[Per_RepIcms] [numeric](7, 4) NULL,
	[Flg_PrcUniAcrIcm] [bit] NULL,
	[Vlr_BasIrf] [numeric](18, 4) NULL,
	[Alq_Irf] [numeric](18, 4) NULL,
	[Vlr_Irf] [numeric](18, 4) NULL,
	[Vlr_SbtRetAnt] [numeric](18, 4) NULL,
	[Vlr_Bonificacao] [numeric](18, 4) NULL,
	[Vlr_BasCsl] [numeric](18, 4) NULL,
	[Alq_Csl] [numeric](7, 4) NULL,
	[Vlr_Csl] [numeric](18, 4) NULL,
	[Flg_TrbFedDedTit] [bit] NULL,
 CONSTRAINT [PK_R_NFSIT] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Ser_Nota] ASC,
	[Num_Nota] ASC,
	[Cod_Produto] ASC,
	[Cod_Lote] ASC,
	[Num_SeqIte] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Cod_Estabe]  DEFAULT (0) FOR [Cod_Estabe]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Num_SeqIte]  DEFAULT (0) FOR [Num_SeqIte]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_Produto]  DEFAULT (0) FOR [Qtd_Produto]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_Bonificacao]  DEFAULT (0) FOR [Qtd_Bonificacao]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_DscItePdv]  DEFAULT (0) FOR [Flg_DscItePdv]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_Desconto]  DEFAULT (0) FOR [Per_Desconto]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_Unitario]  DEFAULT (0) FOR [Prc_Unitario]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_CusIte]  DEFAULT (0) FOR [Vlr_CusIte]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_Icms]  DEFAULT (0) FOR [Alq_Icms]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_IcmsNor]  DEFAULT (0) FOR [Vlr_IcmsNor]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_RepIcms]  DEFAULT (0) FOR [Vlr_RepIcms]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_SubsTrib]  DEFAULT (0) FOR [Vlr_SubsTrib]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_IcmsTri]  DEFAULT (0) FOR [Vlr_IcmsTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasIcmsNor]  DEFAULT (0) FOR [Vlr_BasIcmsNor]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasRepIcms]  DEFAULT (0) FOR [Vlr_BasRepIcms]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_PrdSubTri]  DEFAULT (0) FOR [Vlr_PrdSubTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasSubsTrib]  DEFAULT (0) FOR [Vlr_BasSubsTrib]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasIcmsTri]  DEFAULT (0) FOR [Vlr_BasIcmsTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Isento]  DEFAULT (0) FOR [Vlr_Isento]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Ipi]  DEFAULT (0) FOR [Vlr_Ipi]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DscTri]  DEFAULT (0) FOR [Vlr_DscTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_TotItem]  DEFAULT (0) FOR [Vlr_TotItem]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_LiqItem]  DEFAULT (0) FOR [Vlr_LiqItem]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BruItem]  DEFAULT (0) FOR [Vlr_BruItem]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_BlqDsc]  DEFAULT (0) FOR [Flg_BlqDsc]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_Descon]  DEFAULT (0) FOR [Per_Descon]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComVnd]  DEFAULT (0) FOR [Per_Comissao]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Comissao]  DEFAULT (0) FOR [Vlr_Comissao]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComTlm]  DEFAULT (0) FOR [Per_ComTlmkt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ComTlmkt]  DEFAULT (0) FOR [Vlr_ComTlmkt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_ComisNormal]  DEFAULT (0) FOR [Flg_ComisNormal]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_Tabela]  DEFAULT (0) FOR [Prc_Tabela]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Cod_Cfo]  DEFAULT (0) FOR [Cod_Cfo]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Outros]  DEFAULT (0) FOR [Vlr_Outros]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DespRateada]  DEFAULT (0) FOR [Vlr_DespRateada]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DescRateado]  DEFAULT (0) FOR [Vlr_DescRateado]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_TabBru]  DEFAULT (0) FOR [Prc_TabBru]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_ImpFat]  DEFAULT (0) FOR [Qtd_ImpFat]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_UniImpFat]  DEFAULT (0) FOR [Prc_UniImpFat]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_Pra]  DEFAULT (0) FOR [Qtd_Pra]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_Dep]  DEFAULT (0) FOR [Qtd_Dep]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_MaxCon]  DEFAULT (0) FOR [Prc_MaxCon]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_Fabric]  DEFAULT (0) FOR [Prc_Fabric]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComTra]  DEFAULT (0) FOR [Per_ComTra]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Val_ComTra]  DEFAULT (0) FOR [Val_ComTra]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_BlqInfPar]  DEFAULT (0) FOR [Flg_BlqInfPar]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Fat_CnvImpFat]  DEFAULT (0) FOR [Fat_CnvImpFat]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DescontoFin]  DEFAULT (0) FOR [Per_DescontoFin]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_IncFis]  DEFAULT (0) FOR [Flg_IncFis]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_RedBasCalIcm]  DEFAULT (0) FOR [Per_RedBasCalIcm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_RedBasCalIcmSbt]  DEFAULT (0) FOR [Per_RedBasCalIcmSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_IcmSbt]  DEFAULT (0) FOR [Alq_IcmSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasTri]  DEFAULT (0) FOR [Vlr_BasTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_AgrSbt]  DEFAULT (0) FOR [Alq_AgrSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Frete]  DEFAULT (0) FOR [Vlr_Frete]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Seguro]  DEFAULT (0) FOR [Vlr_Seguro]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasPis]  DEFAULT (0) FOR [Vlr_BasPis]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_Pis]  DEFAULT (0) FOR [Alq_Pis]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Pis]  DEFAULT (0) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasCofins]  DEFAULT (0) FOR [Vlr_BasCofins]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_Cofins]  DEFAULT (0) FOR [Alq_Cofins]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Cofins]  DEFAULT (0) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasRecSbt]  DEFAULT (0) FOR [Vlr_BasRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_RecSbt]  DEFAULT (0) FOR [Vlr_RecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasIpi]  DEFAULT (0) FOR [Vlr_BasIpi]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_Ipi]  DEFAULT (0) FOR [Alq_Ipi]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_SubsTribEmb]  DEFAULT (0) FOR [Vlr_SubsTribEmb]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Id_PolCom]  DEFAULT (0) FOR [Id_PolCom]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_Verba]  DEFAULT (0) FOR [Vlr_Verba]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_PrcBasRecSbt]  DEFAULT (0) FOR [Vlr_PrcBasRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_AgrDebRecSbt]  DEFAULT (0) FOR [Alq_AgrDebRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_RedBasCalIcmDebRecSbt]  DEFAULT (0) FOR [Per_RedBasCalIcmDebRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_IcmDebRecSbt]  DEFAULT (0) FOR [Alq_IcmDebRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasRecSbtInt]  DEFAULT (0) FOR [Vlr_BasRecSbtInt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_RecSbtInt]  DEFAULT (0) FOR [Vlr_RecSbtInt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Val_Descon]  DEFAULT (0) FOR [Val_Descon]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DscPol]  DEFAULT (0) FOR [Per_DscPol]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Val_DscPol]  DEFAULT (0) FOR [Val_DscPol]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DscPrm]  DEFAULT (0) FOR [Per_DscPrm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Val_DscPrm]  DEFAULT (0) FOR [Val_DscPrm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Val_CusTab]  DEFAULT (0) FOR [Val_CusTab]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Val_CusMed]  DEFAULT (0) FOR [Val_CusMed]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Num_Nra]  DEFAULT (0) FOR [Num_Nra]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_PreNra]  DEFAULT (0) FOR [Qtd_PreNra]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Num_ConReg]  DEFAULT (0) FOR [Num_ConReg]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_Cancel]  DEFAULT (0) FOR [Flg_Cancel]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_Prescr]  DEFAULT (0) FOR [Flg_Prescr]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_LiqUltEnt]  DEFAULT (0) FOR [Prc_LiqUltEnt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DescItem]  DEFAULT ((0)) FOR [Vlr_DescItem]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_LiqVenIte]  DEFAULT ((0)) FOR [Vlr_LiqVenIte]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasSbtRes]  DEFAULT ((0)) FOR [Vlr_BasSbtRes]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_SbtRes]  DEFAULT ((0)) FOR [Vlr_SbtRes]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComGer]  DEFAULT ((0)) FOR [Per_ComGer]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ComGer]  DEFAULT ((0)) FOR [Vlr_ComGer]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComSup]  DEFAULT ((0)) FOR [Per_ComSup]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ComSup]  DEFAULT ((0)) FOR [Vlr_ComSup]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComGerOpe]  DEFAULT ((0)) FOR [Per_ComGerOpe]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ComGerOpe]  DEFAULT ((0)) FOR [Vlr_ComGerOpe]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ComSupOpe]  DEFAULT ((0)) FOR [Per_ComSupOpe]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ComSupOpe]  DEFAULT ((0)) FOR [Vlr_ComSupOpe]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_VrbPar]  DEFAULT ((0)) FOR [Per_VrbPar]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_VrbPar]  DEFAULT ((0)) FOR [Vlr_VrbPar]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_VrbBon]  DEFAULT ((0)) FOR [Per_VrbBon]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_VrbBon]  DEFAULT ((0)) FOR [Vlr_VrbBon]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_UniPde]  DEFAULT ((0)) FOR [Prc_UniPde]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DscPde]  DEFAULT ((0)) FOR [Per_DscPde]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_UsaCreIcmSbt]  DEFAULT ((0)) FOR [Flg_UsaCreIcmSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_RefFpb]  DEFAULT ((0)) FOR [Prc_RefFpb]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_PolPrm]  DEFAULT ((0)) FOR [Flg_PolPrm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DscBonRat]  DEFAULT ((0)) FOR [Vlr_DscBonRat]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_CusMinComMarkup]  DEFAULT ((0)) FOR [Prc_CusMinComMarkup]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_VrbVdr]  DEFAULT ((0)) FOR [Vlr_VrbVdr]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_VrbSup]  DEFAULT ((0)) FOR [Vlr_VrbSup]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_UniPdv]  DEFAULT ((0)) FOR [Prc_UniPdv]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DscPdv]  DEFAULT ((0)) FOR [Per_DscPdv]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ResIcmSbtIntSN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_ResIcmSbtExtEN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasResIcmSbtIntSN]  DEFAULT ((0)) FOR [Vlr_BasResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_ResIcmSbtIntSN]  DEFAULT ((0)) FOR [Alq_ResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasResIcmSbtExtEN]  DEFAULT ((0)) FOR [Vlr_BasResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_ResIcmSbtExtEN]  DEFAULT ((0)) FOR [Alq_ResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_MvaResIcmSbtIntSN]  DEFAULT ((0)) FOR [Alq_MvaResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_CusComIte]  DEFAULT ((0.00)) FOR [Vlr_CusComIte]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_AcrAlqIntFcp]  DEFAULT ((0)) FOR [Per_AcrAlqIntFcp]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_PrvParIcm]  DEFAULT ((0)) FOR [Per_PrvParIcm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_IcmFcpDes]  DEFAULT ((0)) FOR [Vlr_IcmFcpDes]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_IcmParDes]  DEFAULT ((0)) FOR [Vlr_IcmParDes]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_IcmParRem]  DEFAULT ((0)) FOR [Vlr_IcmParRem]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Num_NfePrcBasRecSbt]  DEFAULT ((0)) FOR [Num_NfePrcBasRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_ResExt]  DEFAULT ((0)) FOR [Per_ResExt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasDspExt]  DEFAULT ((0)) FOR [Vlr_BasDspExt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DspExt]  DEFAULT ((0)) FOR [Vlr_DspExt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_IntIcmDes]  DEFAULT ((0)) FOR [Alq_IntIcmDes]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Cod_KitPrm]  DEFAULT ((0)) FOR [Cod_KitPrm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasCalSubTriEntMed]  DEFAULT ((0)) FOR [Vlr_BasCalSubTriEntMed]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_IcmPresum]  DEFAULT ((0)) FOR [Alq_IcmPresum]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Flg_UsaDebPresum]  DEFAULT ((0)) FOR [Flg_UsaDebPresum]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_DscBonDup]  DEFAULT ((0)) FOR [Vlr_DscBonDup]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasDscTri]  DEFAULT ((0)) FOR [Vlr_BasDscTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DscTri]  DEFAULT ((0)) FOR [Per_DscTri]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_BasVerba]  DEFAULT ((0)) FOR [Vlr_BasVerba]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Prc_PolCom]  DEFAULT ((0)) FOR [Prc_PolCom]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_DscPolCom]  DEFAULT ((0)) FOR [Per_DscPolCom]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Alq_IcmDif]  DEFAULT ((0)) FOR [Alq_IcmDif]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Vlr_IcmsDif]  DEFAULT ((0)) FOR [Vlr_IcmsDif]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Qtd_PesVolImpFat]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Prc_UniPesVolImpFat]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmsDeson]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_DscCalSuframa]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_FcpIcm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbtRec]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRec]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Per_IcmDiferi]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmDiferi]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_DspDevST]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_PisEstDev]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_CofEstDev]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_LucLiq]  DEFAULT ((0)) FOR [Per_LucLiq]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Per_LucBru]  DEFAULT ((0)) FOR [Per_LucBru]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_PrcBasDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_AgrDebDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Per_RedBasCalIcmDebDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_IcmDebDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_BasDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_DevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Prc_PonFin]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  CONSTRAINT [DF_R_NFSIT_Qtd_PesLiq]  DEFAULT ((0)) FOR [Qtd_PesLiq]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Prc_UniSemAcrIcm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Prc_UniComAcrIcm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Per_RepIcms]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Flg_PrcUniAcrIcm]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_BasIrf]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_Irf]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_Irf]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_SbtRetAnt]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_Bonificacao]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_BasCsl]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Alq_Csl]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Vlr_Csl]
GO

ALTER TABLE [dbo].[R_NFSIT] ADD  DEFAULT ((0)) FOR [Flg_TrbFedDedTit]
GO

ALTER TABLE [dbo].[R_NFSIT]  WITH NOCHECK ADD  CONSTRAINT [FK_R_NFSIT_R_NFSCB] FOREIGN KEY([Cod_Estabe], [Ser_Nota], [Num_Nota])
REFERENCES [dbo].[R_NFSCB] ([Cod_Estabe], [Ser_Nota], [Num_Nota])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_NFSIT] CHECK CONSTRAINT [FK_R_NFSIT_R_NFSCB]
GO

ALTER TABLE [dbo].[R_NFSIT]  WITH NOCHECK ADD  CONSTRAINT [FK_R_NFSIT_R_PRODU] FOREIGN KEY([Cod_Produto])
REFERENCES [dbo].[R_PRODU] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_NFSIT] CHECK CONSTRAINT [FK_R_NFSIT_R_PRODU]
GO


