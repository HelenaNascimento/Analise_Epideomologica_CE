USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_PDVIT]    Script Date: 28/06/2024 08:45:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_PDVIT](
	[Cod_Pedido] [int] NOT NULL,
	[Cod_Produto] [int] NOT NULL,
	[Cod_Lote] [varchar](20) NOT NULL,
	[Cod_Fabricante] [int] NULL,
	[Loc_Fisica] [varchar](15) NULL,
	[Qtd_Solicitado] [int] NULL,
	[Qtd_Acerto] [int] NULL,
	[Qtd_Pedido] [int] NULL,
	[Qtd_Bonificacao] [int] NULL,
	[Qtd_Pendente] [int] NULL,
	[Flg_PrcUniDsc] [bit] NULL,
	[Per_Desconto] [numeric](18, 8) NULL,
	[Prc_Unitario] [numeric](18, 8) NULL,
	[Tip_Aux] [varchar](1) NULL,
	[Ctrl_Tributacao] [varchar](2) NULL,
	[Cod_ClaTri] [varchar](4) NULL,
	[Alq_Icms] [numeric](18, 8) NULL,
	[Vlr_BasIcmsNor] [numeric](18, 8) NULL,
	[Vlr_IcmsNor] [numeric](18, 8) NULL,
	[Vlr_RepIcms] [numeric](18, 8) NULL,
	[Vlr_BasRepIcms] [numeric](18, 8) NULL,
	[Vlr_SubsTrib] [numeric](18, 8) NULL,
	[Vlr_PrdSubTri] [numeric](18, 8) NULL,
	[Vlr_BasSubsTrib] [numeric](18, 8) NULL,
	[Vlr_IcmsTri] [numeric](18, 8) NULL,
	[Vlr_BasIcmsTri] [numeric](18, 8) NULL,
	[Vlr_Isento] [numeric](18, 8) NULL,
	[Vlr_DscTri] [numeric](18, 8) NULL,
	[Vlr_Bruto] [numeric](18, 8) NULL,
	[Cod_Promocao] [int] NULL,
	[Desconto] [bit] NULL,
	[Flg_BlqInfPar] [bit] NULL,
	[Qtd_PrzMax] [int] NULL,
	[Per_Descon] [numeric](18, 8) NULL,
	[Per_DscVis] [numeric](18, 8) NULL,
	[C_VlrDesconto] [numeric](18, 4) NULL,
	[C_PrcUni] [numeric](18, 8) NULL,
	[C_PerDscIte] [numeric](18, 4) NULL,
	[C_PrcTotal] [numeric](18, 4) NULL,
	[C_VlrLiquido] [numeric](18, 4) NULL,
	[Prc_Tabela] [numeric](18, 4) NULL,
	[Des_Und] [varchar](3) NULL,
	[Qtd_AprPdv] [int] NULL,
	[Des_UndVen] [varchar](12) NULL,
	[Prc_TabBru] [numeric](18, 4) NULL,
	[Vlr_Outros] [numeric](18, 4) NULL,
	[Flg_Entreg] [bit] NULL,
	[Qtd_ImpFat] [numeric](18, 4) NULL,
	[Prc_UniImpFat] [numeric](18, 8) NULL,
	[Fat_CnvImpFat] [int] NULL,
	[Qtd_Pra] [int] NULL,
	[Qtd_Dep] [int] NULL,
	[Prc_MaxCon] [numeric](18, 4) NULL,
	[Prc_Fabric] [numeric](18, 4) NULL,
	[Num_SeqDig] [int] NULL,
	[Nom_UsuDesbloq] [varchar](15) NULL,
	[Des_UnvImpFat] [char](3) NULL,
	[Des_AprImpFat] [varchar](12) NULL,
	[Flg_VctLic] [bit] NULL,
	[Per_Rnt] [numeric](18, 4) NULL,
	[Prc_BasRnt] [numeric](18, 4) NULL,
	[Per_Comissao] [numeric](18, 4) NULL,
	[Per_ComTlmkt] [numeric](18, 4) NULL,
	[Per_ComTra] [numeric](18, 4) NULL,
	[Vlr_Comissao] [numeric](18, 4) NULL,
	[Vlr_ComTlmkt] [numeric](18, 4) NULL,
	[Val_ComTra] [numeric](18, 4) NULL,
	[Cod_ModBasCalIcm] [char](1) NULL,
	[Cod_ModBasCalIcmSbt] [char](1) NULL,
	[Per_RedBasCalIcm] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmSbt] [numeric](18, 4) NULL,
	[Alq_IcmSbt] [numeric](18, 4) NULL,
	[Vlr_BasTri] [numeric](18, 8) NULL,
	[Alq_AgrSbt] [numeric](18, 4) NULL,
	[Id_PolCom] [int] NULL,
	[Flg_Verba] [bit] NULL,
	[Prc_PolCom] [numeric](18, 8) NULL,
	[Per_DscPolCom] [numeric](18, 4) NULL,
	[Per_DscVisPolCom] [numeric](18, 4) NULL,
	[Vlr_Verba] [numeric](18, 4) NULL,
	[Qtd_PrzPolCom] [int] NULL,
	[Des_MsgDesbloq] [varchar](60) NULL,
	[Prc_UniLiqPer] [numeric](18, 8) NULL,
	[Prc_LiqUltEnt] [numeric](18, 8) NULL,
	[Cod_MtvRej] [varchar](3) NULL,
	[Des_MtvRej] [varchar](80) NULL,
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
	[Prc_RefFpb] [numeric](18, 4) NULL,
	[Cod_CSOSN] [varchar](3) NULL,
	[Flg_PolPrm] [bit] NULL,
	[Cod_PrjPde] [varchar](12) NULL,
	[Num_SeqPdc] [varchar](6) NULL,
	[Vlr_BasSbtRes] [numeric](18, 8) NULL,
	[Vlr_SbtRes] [numeric](18, 8) NULL,
	[Num_IteCot] [varchar](6) NULL,
	[Vlr_Bonificacao] [numeric](18, 4) NULL,
	[Vlr_DscBonRat] [numeric](18, 4) NULL,
	[Prc_CusMinComMarkup] [numeric](18, 4) NULL,
	[Vlr_BasVerba] [numeric](18, 4) NULL,
	[Vlr_VrbVdr] [numeric](18, 4) NULL,
	[Per_MarkupCusCom] [numeric](18, 4) NULL,
	[Tip_Sai] [varchar](1) NOT NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Per_AcrAlqIntFcp] [numeric](18, 4) NULL,
	[Vlr_IcmFcpDes] [numeric](18, 4) NULL,
	[Vlr_Despes] [numeric](18, 4) NULL,
	[Vlr_BasDespes] [numeric](18, 4) NULL,
	[Per_PrvParIcm] [numeric](18, 4) NULL,
	[Vlr_IcmParDes] [numeric](18, 4) NULL,
	[Vlr_IcmParRem] [numeric](18, 4) NULL,
	[Alq_IntIcmDes] [numeric](18, 4) NULL,
	[Cod_KitPrm] [int] NULL,
	[CST_Pis] [varchar](2) NULL,
	[Vlr_BasPis] [numeric](18, 4) NULL,
	[Alq_Pis] [numeric](18, 4) NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[CST_Cofins] [varchar](2) NULL,
	[Vlr_BasCofins] [numeric](18, 4) NULL,
	[Alq_Cofins] [numeric](18, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[Per_MarBru] [numeric](18, 4) NULL,
	[Vlr_VrbPdv] [numeric](18, 4) NULL,
	[Vlr_BasCalSubTriEntMed] [numeric](18, 4) NULL,
	[Per_RntBru] [numeric](18, 4) NULL,
	[Tip_BasRnt] [varchar](1) NULL,
	[Flg_UsaDebPresum] [bit] NULL,
	[Alq_IcmPresum] [numeric](18, 4) NULL,
	[Vlr_DscBonDup] [numeric](18, 4) NULL,
	[Vlr_BasDscTri] [numeric](18, 4) NULL,
	[Per_DscTri] [numeric](9, 4) NULL,
	[Alq_IcmDif] [numeric](9, 4) NULL,
	[Vlr_IcmsDif] [numeric](18, 4) NULL,
	[Qtd_PesVolImpFat] [numeric](18, 4) NOT NULL,
	[Des_UnvPesVolImpFat] [varchar](3) NULL,
	[Prc_UniPesVolImpFat] [numeric](18, 8) NOT NULL,
	[Vlr_IcmsDeson] [numeric](18, 4) NOT NULL,
	[Cod_MtvIcmsDeson] [varchar](2) NULL,
	[Vlr_DscCalSuframa] [numeric](18, 4) NOT NULL,
	[Flg_IncFis] [bit] NOT NULL,
	[Alq_FcpIcm] [numeric](18, 4) NULL,
	[Vlr_FcpIcm] [numeric](18, 4) NULL,
	[Alq_FcpSbt] [numeric](18, 4) NULL,
	[Vlr_FcpSbt] [numeric](18, 4) NULL,
	[Alq_FcpSbtRet] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRet] [numeric](18, 4) NULL,
	[Per_IcmDiferi] [numeric](8, 4) NOT NULL,
	[Vlr_IcmDiferi] [numeric](18, 4) NOT NULL,
	[Nom_UsuCor] [varchar](15) NULL,
	[Dat_UsuCor] [datetime] NULL,
	[Cod_MtvCor] [varchar](3) NULL,
	[Des_MtvCor] [varchar](40) NULL,
	[Vlr_VrbOpe] [numeric](18, 4) NULL,
	[Vlr_VrbSup] [numeric](18, 4) NULL,
	[Per_VrbVdr] [numeric](5, 2) NULL,
	[Per_VrbOpe] [numeric](5, 2) NULL,
	[Per_VrbSup] [numeric](5, 2) NULL,
	[Alq_EfeDas] [numeric](7, 4) NULL,
	[Vlr_DscRat] [numeric](18, 4) NULL,
	[Prc_PonFin] [numeric](18, 4) NULL,
	[Vlr_DspExt] [numeric](18, 4) NULL,
	[Vlr_BasIpi] [numeric](18, 4) NULL,
	[Vlr_Ipi] [numeric](18, 4) NULL,
	[Alq_Ipi] [numeric](7, 4) NULL,
	[CST_Ipi] [varchar](2) NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Cod_OriMer] [varchar](3) NULL,
	[Prc_UniSemAcrIcm] [numeric](18, 8) NULL,
	[Flg_PrcUniAcrIcm] [bit] NULL,
	[Prc_UniComAcrIcm] [numeric](18, 8) NULL,
	[Per_RepIcms] [numeric](7, 4) NULL,
	[Vlr_BasIrf] [numeric](18, 4) NULL,
	[Alq_Irf] [numeric](18, 4) NULL,
	[Vlr_Irf] [numeric](18, 4) NULL,
	[Qtd_GanCot] [int] NULL,
	[Per_LinCot] [numeric](8, 4) NULL,
	[Vlr_SbtRetAnt] [numeric](18, 4) NULL,
	[Vlr_BasCsl] [numeric](18, 4) NULL,
	[Alq_Csl] [numeric](7, 4) NULL,
	[Vlr_Csl] [numeric](18, 4) NULL,
	[Tip_PolCom] [varchar](4) NULL,
 CONSTRAINT [PK_R_PDVIT] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Cod_Pedido] ASC,
	[Cod_Produto] ASC,
	[Cod_Lote] ASC,
	[Tip_Sai] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Solicitado]  DEFAULT (0) FOR [Qtd_Solicitado]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Acerto]  DEFAULT (0) FOR [Qtd_Acerto]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Pedido]  DEFAULT (0) FOR [Qtd_Pedido]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Bonificacao]  DEFAULT (0) FOR [Qtd_Bonificacao]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Pendente]  DEFAULT (0) FOR [Qtd_Pendente]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_PrcUniDsc]  DEFAULT (0) FOR [Flg_PrcUniDsc]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_Desconto]  DEFAULT (0) FOR [Per_Desconto]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_Unitario]  DEFAULT (0) FOR [Prc_Unitario]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_Icms]  DEFAULT (0) FOR [Alq_Icms]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasIcmsNor]  DEFAULT (0) FOR [Vlr_BasIcmsNor]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_IcmsNor]  DEFAULT (0) FOR [Vlr_IcmsNor]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_RepIcms]  DEFAULT (0) FOR [Vlr_RepIcms]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasRepIcms]  DEFAULT (0) FOR [Vlr_BasRepIcms]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_SubsTrib]  DEFAULT (0) FOR [Vlr_SubsTrib]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_PrdSubTri]  DEFAULT (0) FOR [Vlr_PrdSubTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasSubsTrib]  DEFAULT (0) FOR [Vlr_BasSubsTrib]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_IcmsTri]  DEFAULT (0) FOR [Vlr_IcmsTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasIcmsTri]  DEFAULT (0) FOR [Vlr_BasIcmsTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Isento]  DEFAULT (0) FOR [Vlr_Isento]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_DscTri]  DEFAULT (0) FOR [Vlr_DscTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Bruto]  DEFAULT (0) FOR [Vlr_Bruto]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Desconto]  DEFAULT (0) FOR [Desconto]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_BlqInfPar]  DEFAULT (0) FOR [Flg_BlqInfPar]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_PrzMax]  DEFAULT (0) FOR [Qtd_PrzMax]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_Descon]  DEFAULT (0) FOR [Per_Descon]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_DscVis]  DEFAULT (0) FOR [Per_DscVis]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_C_VlrDesconto]  DEFAULT (0) FOR [C_VlrDesconto]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_C_PrcUni]  DEFAULT (0) FOR [C_PrcUni]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_C_PerDscIte]  DEFAULT (0) FOR [C_PerDscIte]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_C_PrcTotal]  DEFAULT (0) FOR [C_PrcTotal]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_C_VlrLiquido]  DEFAULT (0) FOR [C_VlrLiquido]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_Tabela]  DEFAULT (0) FOR [Prc_Tabela]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_AprPdv]  DEFAULT (0) FOR [Qtd_AprPdv]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_TabBru]  DEFAULT (0) FOR [Prc_TabBru]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Outros]  DEFAULT (0) FOR [Vlr_Outros]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_Entreg]  DEFAULT (0) FOR [Flg_Entreg]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_ImpFat]  DEFAULT (0) FOR [Qtd_ImpFat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_UniImpFat]  DEFAULT (0) FOR [Prc_UniImpFat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Fat_CnvImpFat]  DEFAULT (0) FOR [Fat_CnvImpFat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Pra]  DEFAULT (0) FOR [Qtd_Pra]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_Dep]  DEFAULT (0) FOR [Qtd_Dep]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_MaxCon]  DEFAULT (0) FOR [Prc_MaxCon]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_Fabric]  DEFAULT (0) FOR [Prc_Fabric]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Num_SeqDig]  DEFAULT (0) FOR [Num_SeqDig]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_VctLic]  DEFAULT (0) FOR [Flg_VctLic]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_Rnt]  DEFAULT (0) FOR [Per_Rnt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_BasRnt]  DEFAULT (0) FOR [Prc_BasRnt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_Comissao]  DEFAULT (0) FOR [Per_Comissao]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_ComTlmkt]  DEFAULT (0) FOR [Per_ComTlmkt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_ComTra]  DEFAULT (0) FOR [Per_ComTra]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Comissao]  DEFAULT (0) FOR [Vlr_Comissao]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_ComTlmkt]  DEFAULT (0) FOR [Vlr_ComTlmkt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Val_ComTra]  DEFAULT (0) FOR [Val_ComTra]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_RedBasCalIcm]  DEFAULT (0) FOR [Per_RedBasCalIcm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_RedBasCalIcmSbt]  DEFAULT (0) FOR [Per_RedBasCalIcmSbt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_IcmSbt]  DEFAULT (0) FOR [Alq_IcmSbt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasTri]  DEFAULT (0) FOR [Vlr_BasTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_AgrSbt]  DEFAULT (0) FOR [Alq_AgrSbt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Id_PolCom]  DEFAULT (0) FOR [Id_PolCom]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_Verba]  DEFAULT (0) FOR [Flg_Verba]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_PolCom]  DEFAULT (0) FOR [Prc_PolCom]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_DscPolCom]  DEFAULT (0) FOR [Per_DscPolCom]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_DscVisPolCom]  DEFAULT (0) FOR [Per_DscVisPolCom]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Verba]  DEFAULT (0) FOR [Vlr_Verba]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Qtd_PrzPolCom]  DEFAULT (0) FOR [Qtd_PrzPolCom]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_UniLiqPer]  DEFAULT (0) FOR [Prc_UniLiqPer]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_LiqUltEnt]  DEFAULT (0) FOR [Prc_LiqUltEnt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_ComGer]  DEFAULT ((0)) FOR [Per_ComGer]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_ComGer]  DEFAULT ((0)) FOR [Vlr_ComGer]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_ComSup]  DEFAULT ((0)) FOR [Per_ComSup]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_ComSup]  DEFAULT ((0)) FOR [Vlr_ComSup]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_ComGerOpe]  DEFAULT ((0)) FOR [Per_ComGerOpe]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_ComGerOpe]  DEFAULT ((0)) FOR [Vlr_ComGerOpe]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_ComSupOpe]  DEFAULT ((0)) FOR [Per_ComSupOpe]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_ComSupOpe]  DEFAULT ((0)) FOR [Vlr_ComSupOpe]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_VrbPar]  DEFAULT ((0)) FOR [Per_VrbPar]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_VrbPar]  DEFAULT ((0)) FOR [Vlr_VrbPar]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_VrbBon]  DEFAULT ((0)) FOR [Per_VrbBon]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_VrbBon]  DEFAULT ((0)) FOR [Vlr_VrbBon]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_UniPde]  DEFAULT ((0)) FOR [Prc_UniPde]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_DscPde]  DEFAULT ((0)) FOR [Per_DscPde]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_RefFpb]  DEFAULT ((0)) FOR [Prc_RefFpb]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_PolPrm]  DEFAULT ((0)) FOR [Flg_PolPrm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasSbtRes]  DEFAULT ((0)) FOR [Vlr_BasSbtRes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_SbtRes]  DEFAULT ((0)) FOR [Vlr_SbtRes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Bonificacao]  DEFAULT ((0)) FOR [Vlr_Bonificacao]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_DscBonRat]  DEFAULT ((0)) FOR [Vlr_DscBonRat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Prc_CusMinComMarkup]  DEFAULT ((0)) FOR [Prc_CusMinComMarkup]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasVerba]  DEFAULT ((0)) FOR [Vlr_BasVerba]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_VrbVdr]  DEFAULT ((0)) FOR [Vlr_VrbVdr]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_MarkupCusCom]  DEFAULT ((0)) FOR [Per_MarkupCusCom]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Cod_Estabe]  DEFAULT ((0)) FOR [Cod_Estabe]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_AcrAlqIntFcp]  DEFAULT ((0)) FOR [Per_AcrAlqIntFcp]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_IcmFcpDes]  DEFAULT ((0)) FOR [Vlr_IcmFcpDes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Despes]  DEFAULT ((0)) FOR [Vlr_Despes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasDespes]  DEFAULT ((0)) FOR [Vlr_BasDespes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_PrvParIcm]  DEFAULT ((0)) FOR [Per_PrvParIcm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_IcmParDes]  DEFAULT ((0)) FOR [Vlr_IcmParDes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_IcmParRem]  DEFAULT ((0)) FOR [Vlr_IcmParRem]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_IntIcmDes]  DEFAULT ((0)) FOR [Alq_IntIcmDes]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Cod_KitPrm]  DEFAULT ((0)) FOR [Cod_KitPrm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasPis]  DEFAULT ((0)) FOR [Vlr_BasPis]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_Pis]  DEFAULT ((0)) FOR [Alq_Pis]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Pis]  DEFAULT ((0)) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasCofins]  DEFAULT ((0)) FOR [Vlr_BasCofins]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_Cofins]  DEFAULT ((0)) FOR [Alq_Cofins]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_Cofins]  DEFAULT ((0)) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_MarBru]  DEFAULT ((0)) FOR [Per_MarBru]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_VrbPdv]  DEFAULT ((0)) FOR [Vlr_VrbPdv]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasCalSubTriEntMed]  DEFAULT ((0)) FOR [Vlr_BasCalSubTriEntMed]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_RntBru]  DEFAULT ((0)) FOR [Per_RntBru]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Flg_UsaDebPresum]  DEFAULT ((0)) FOR [Flg_UsaDebPresum]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_IcmPresum]  DEFAULT ((0)) FOR [Alq_IcmPresum]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_DscBonDup]  DEFAULT ((0)) FOR [Vlr_DscBonDup]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_BasDscTri]  DEFAULT ((0)) FOR [Vlr_BasDscTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_DscTri]  DEFAULT ((0)) FOR [Per_DscTri]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Alq_IcmDif]  DEFAULT ((0)) FOR [Alq_IcmDif]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Vlr_IcmsDif]  DEFAULT ((0)) FOR [Vlr_IcmsDif]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Qtd_PesVolImpFat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Prc_UniPesVolImpFat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmsDeson]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_DscCalSuframa]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Flg_IncFis]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Alq_FcpIcm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Per_IcmDiferi]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmDiferi]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_VrbVdr]  DEFAULT ((0)) FOR [Per_VrbVdr]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_VrbOpe]  DEFAULT ((0)) FOR [Per_VrbOpe]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  CONSTRAINT [DF_R_PDVIT_Per_VrbSup]  DEFAULT ((0)) FOR [Per_VrbSup]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_DscRat]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Prc_PonFin]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_DspExt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_BasIpi]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_Ipi]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Alq_Ipi]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Prc_UniSemAcrIcm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Flg_PrcUniAcrIcm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Prc_UniComAcrIcm]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Per_RepIcms]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_BasIrf]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Alq_Irf]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_Irf]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Qtd_GanCot]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Per_LinCot]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_SbtRetAnt]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_BasCsl]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Alq_Csl]
GO

ALTER TABLE [dbo].[R_PDVIT] ADD  DEFAULT ((0)) FOR [Vlr_Csl]
GO

ALTER TABLE [dbo].[R_PDVIT]  WITH NOCHECK ADD  CONSTRAINT [FK_R_PDVIT_PRODU] FOREIGN KEY([Cod_Produto])
REFERENCES [dbo].[R_PRODU] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_PDVIT] CHECK CONSTRAINT [FK_R_PDVIT_PRODU]
GO


