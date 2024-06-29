USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_NFEIT]    Script Date: 28/06/2024 08:50:52 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_NFEIT](
	[Protocolo] [int] NOT NULL,
	[Cod_Produto] [int] NOT NULL,
	[Cod_Lote] [varchar](20) NOT NULL,
	[Num_SeqIte] [smallint] NOT NULL,
	[Dat_Movimento] [smalldatetime] NULL,
	[Dat_Vencim] [smalldatetime] NULL,
	[Qtd_Pedido] [int] NULL,
	[Qtd_Bonificacao] [int] NULL,
	[Qtd_PedFat] [int] NULL,
	[Qtd_BonFat] [int] NULL,
	[Flg_ComisNormal] [bit] NULL,
	[Prc_UniFat] [numeric](18, 8) NULL,
	[Prc_Unitario] [numeric](18, 8) NULL,
	[Per_DescItem] [numeric](18, 8) NULL,
	[Ctrl_Tributacao] [char](2) NULL,
	[Cod_ClaTri] [varchar](4) NULL,
	[Vlr_BasIcmsNor] [numeric](18, 4) NULL,
	[Alq_Icms] [numeric](18, 8) NULL,
	[Vlr_IcmsNor] [numeric](18, 4) NULL,
	[Vlr_Ipi] [numeric](18, 4) NULL,
	[Vlr_IcmsTrib] [numeric](18, 4) NULL,
	[Vlr_SubsTrib] [numeric](18, 4) NULL,
	[Vlr_AntRec] [numeric](18, 4) NULL,
	[Vlr_DespRateada] [numeric](18, 4) NULL,
	[Vlr_DescRateado] [numeric](18, 4) NULL,
	[Vlr_BasIpi] [numeric](18, 4) NULL,
	[Vlr_BasIcmsTrib] [numeric](18, 4) NULL,
	[Vlr_PrdSubTri] [numeric](18, 4) NULL,
	[Vlr_BasSubsTrib] [numeric](18, 4) NULL,
	[Vlr_PrdAntRec] [numeric](18, 4) NULL,
	[Vlr_BasAntRec] [numeric](18, 4) NULL,
	[Vlr_Isento] [numeric](18, 4) NULL,
	[Vlr_LiqIte] [numeric](18, 4) NULL,
	[Vlr_Comissao] [numeric](18, 4) NULL,
	[Vlr_ComTlmkt] [numeric](18, 4) NULL,
	[Cus_LiqUni] [numeric](18, 4) NULL,
	[Prc_CustoMedio] [numeric](18, 4) NULL,
	[Prc_Venda] [numeric](18, 4) NULL,
	[FATOR_FAT] [int] NULL,
	[Cod_Cfo] [smallint] NULL,
	[Vlr_BasDifTri] [numeric](18, 4) NULL,
	[Vlr_DifTri] [numeric](18, 4) NULL,
	[Vlr_Outros] [numeric](18, 4) NULL,
	[Vlr_BasRepIcm] [numeric](18, 4) NULL,
	[Vlr_RepIcm] [numeric](18, 4) NULL,
	[Vlr_Frete] [numeric](18, 4) NULL,
	[Flg_PrcVen] [bit] NULL,
	[Per_Markup] [numeric](18, 8) NULL,
	[Val_PrcVen] [numeric](18, 4) NULL,
	[Flg_DbqPrdBlq] [bit] NULL,
	[Prc_MaxCon] [numeric](18, 4) NULL,
	[Prc_Fabric] [numeric](18, 4) NULL,
	[Num_SeqDig] [int] NULL,
	[Qtd_Promocao] [int] NULL,
	[Transacao] [smalldatetime] NULL,
	[Qtd_EmbFec] [int] NULL,
	[Vlr_DspExtNotFis] [numeric](18, 4) NULL,
	[Vlr_DscExtNotFis] [numeric](18, 4) NULL,
	[Qtd_FisAnt] [int] NULL,
	[Prc_CusMedAnt] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmDeb] [numeric](18, 4) NULL,
	[Alq_AgrDeb] [numeric](18, 4) NULL,
	[Per_RedBasCalDeb] [numeric](18, 4) NULL,
	[Alq_IcmDeb] [numeric](18, 4) NULL,
	[Vlr_DebIcm] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmCre] [numeric](18, 4) NULL,
	[Per_LimBasCalCreSbt] [numeric](18, 4) NULL,
	[Vlr_LimCreIcm] [numeric](18, 4) NULL,
	[Cod_ModBasCalIcm] [char](1) NULL,
	[Cod_ModBasCalIcmSbt] [char](1) NULL,
	[Dat_Fabric] [smalldatetime] NULL,
	[Vlr_BasTri] [numeric](18, 8) NULL,
	[Per_RedBasCalIcm] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmSbt] [numeric](18, 4) NULL,
	[Alq_IcmSbt] [numeric](18, 4) NULL,
	[Alq_AgrSbt] [numeric](18, 4) NULL,
	[Vlr_Seguro] [numeric](18, 4) NULL,
	[Vlr_BasPis] [numeric](18, 4) NULL,
	[Alq_Pis] [numeric](18, 4) NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[Vlr_BasCofins] [numeric](18, 4) NULL,
	[Alq_Cofins] [numeric](18, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[CST_Pis] [varchar](2) NULL,
	[CST_Cofins] [varchar](2) NULL,
	[Tip_LisPis] [char](1) NULL,
	[Alq_Ipi] [numeric](18, 4) NULL,
	[CST_Ipi] [varchar](2) NULL,
	[Vlr_Verba] [numeric](18, 4) NULL,
	[Vlr_PrcBasRecSbt] [numeric](18, 8) NULL,
	[Alq_AgrDebRecSbt] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmDebRecSbt] [numeric](18, 4) NULL,
	[Alq_IcmDebRecSbt] [numeric](18, 4) NULL,
	[Flg_RegEspTrb] [bit] NULL,
	[Cod_Tribut] [varchar](3) NULL,
	[Vlr_DspSbt] [numeric](18, 4) NULL,
	[Vlr_BasSbtRes] [numeric](18, 4) NULL,
	[Vlr_SbtRes] [numeric](18, 4) NULL,
	[Per_IpiCad] [numeric](18, 4) NULL,
	[Vlr_Suframa] [numeric](18, 4) NULL,
	[Flg_UsaCreIcmSbt] [bit] NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Prc_Unitar] [numeric](18, 8) NULL,
	[Prc_RefFpb] [numeric](18, 4) NULL,
	[Cod_CSOSN] [varchar](3) NULL,
	[Vlr_PrcBasDevRecSbt] [numeric](18, 4) NULL,
	[Alq_AgrDebDevRecSbt] [numeric](18, 4) NULL,
	[Per_RedBasCalIcmDebDevRecSbt] [numeric](18, 4) NULL,
	[Alq_IcmDebDevRecSbt] [numeric](18, 4) NULL,
	[Vlr_BasDevRecSbt] [numeric](18, 4) NULL,
	[Vlr_DevRecSbt] [numeric](18, 4) NULL,
	[Vlr_DscIte1] [numeric](18, 4) NULL,
	[Vlr_DscIte2] [numeric](18, 4) NULL,
	[Vlr_BruItem] [numeric](18, 4) NULL,
	[Vlr_TotItem] [numeric](18, 4) NULL,
	[Vlr_DescItem] [numeric](18, 4) NULL,
	[Vlr_LiqCmpIte] [numeric](18, 4) NULL,
	[Vlr_CrePreIcmSbtExt] [numeric](18, 4) NULL,
	[Vlr_ResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_ResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Tip_PrcBasDebSbt] [varchar](1) NULL,
	[Tip_PrcBasCreSbt] [varchar](1) NULL,
	[Vlr_BasCrePreIcmSbtExt] [numeric](18, 4) NULL,
	[Alq_CrePreIcmSbtExt] [numeric](18, 4) NULL,
	[Vlr_BasResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Alq_ResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_BasResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Alq_ResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Alq_MvaResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Flg_BlqDsc] [bit] NULL,
	[Num_NfePrcBasRecSbt] [int] NULL,
	[Cod_Ncm] [varchar](8) NULL,
	[Per_AcrAlqIntFcp] [numeric](18, 4) NULL,
	[Per_PrvParIcm] [numeric](18, 4) NULL,
	[Vlr_IcmFcpDes] [numeric](18, 4) NULL,
	[Vlr_IcmParDes] [numeric](18, 4) NULL,
	[Vlr_IcmParRem] [numeric](18, 4) NULL,
	[Per_ResExt] [numeric](18, 4) NULL,
	[Vlr_BasDspExt] [numeric](18, 4) NULL,
	[Vlr_DspExt] [numeric](18, 4) NULL,
	[Cod_CEST] [varchar](7) NULL,
	[Alq_IntIcmDes] [numeric](18, 4) NULL,
	[Vlr_DspCusEnt] [numeric](18, 4) NULL,
	[Vlr_BasCalSubTriEntMedAnt] [numeric](18, 4) NULL,
	[Alq_IcmPresum] [numeric](18, 4) NULL,
	[Flg_UsaCrePresum] [bit] NULL,
	[Qtd_Avaria] [int] NULL,
	[Tip_Ent] [varchar](1) NULL,
	[Dat_RefPrdCip] [datetime] NULL,
	[Per_DscNegFor] [numeric](18, 4) NULL,
	[Alq_IcmDif] [numeric](9, 4) NULL,
	[Vlr_IcmsDif] [numeric](18, 4) NULL,
	[Vlr_IcmsDeson] [numeric](18, 4) NOT NULL,
	[Cod_MtvIcmsDeson] [varchar](2) NULL,
	[Vlr_DscCalSuframa] [numeric](18, 4) NOT NULL,
	[Alq_FcpIcm] [numeric](18, 4) NULL,
	[Vlr_FcpIcm] [numeric](18, 4) NULL,
	[Alq_FcpSbt] [numeric](18, 4) NULL,
	[Vlr_FcpSbt] [numeric](18, 4) NULL,
	[Alq_FcpSbtRet] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRet] [numeric](18, 4) NULL,
	[Alq_FcpSbtRec] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRec] [numeric](18, 4) NULL,
	[Alq_FcpSbtAnt] [numeric](18, 4) NULL,
	[Vlr_FcpSbtAnt] [numeric](18, 4) NULL,
	[Per_IcmDiferi] [numeric](8, 4) NULL,
	[Vlr_IcmDiferi] [numeric](18, 4) NULL,
	[Xml_vBCSTRet] [numeric](18, 4) NULL,
	[Xml_pST] [numeric](8, 4) NULL,
	[Xml_vICMSSubstituto] [numeric](18, 4) NULL,
	[Xml_vICMSSTRet] [numeric](18, 4) NULL,
	[Xml_vBCFCPSTRet] [numeric](18, 4) NULL,
	[Xml_pFCPSTRet] [numeric](8, 4) NULL,
	[Xml_vFCPSTRet] [numeric](18, 4) NULL,
	[Xml_pRedBCEfet] [numeric](8, 4) NULL,
	[Xml_vBCEfet] [numeric](18, 4) NULL,
	[Xml_pICMSEfet] [numeric](8, 4) NULL,
	[Xml_vICMSEfet] [numeric](18, 4) NULL,
	[Vlr_DspDevST] [numeric](18, 4) NULL,
	[cBenef] [varchar](10) NULL,
	[Vlr_PisEstDev] [numeric](18, 4) NULL,
	[Vlr_CofEstDev] [numeric](18, 4) NULL,
	[Vlr_DscTri] [numeric](18, 4) NULL,
	[Vlr_VrbVdr] [numeric](18, 4) NULL,
	[Vlr_VrbOpe] [numeric](18, 4) NULL,
	[Vlr_VrbSup] [numeric](18, 4) NULL,
	[Qtd_Faltas] [int] NULL,
	[Vlr_FreCte] [numeric](18, 4) NULL,
	[Vlr_IcmRetXml] [numeric](18, 4) NULL,
	[Vlr_BasIcmRetXml] [numeric](18, 4) NULL,
	[Prc_PonFin] [numeric](18, 4) NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Cod_OriMer] [varchar](3) NULL,
	[Qtd_PesLiq] [numeric](18, 4) NULL,
	[Per_RepIcm] [numeric](7, 4) NULL,
	[Vlr_ComSup] [numeric](18, 4) NULL,
	[Vlr_ComSupOpe] [numeric](18, 4) NULL,
	[Vlr_ComGer] [numeric](18, 4) NULL,
	[Vlr_ComGerOpe] [numeric](18, 4) NULL,
	[Id_DclImp] [int] NULL,
	[Imp_nAdicao] [smallint] NULL,
	[Imp_nSeqAdic] [smallint] NULL,
	[Imp_vBCII] [numeric](18, 4) NULL,
	[Imp_vDespAdu] [numeric](18, 4) NULL,
	[Imp_vII] [numeric](18, 4) NULL,
	[Imp_vIOF] [numeric](18, 4) NULL,
	[Imp_vMLE] [numeric](18, 4) NULL,
	[Imp_vFrete] [numeric](18, 4) NULL,
	[Imp_vSeguro] [numeric](18, 4) NULL,
	[Imp_vMLD] [numeric](18, 4) NULL,
	[Imp_vSiscomex] [numeric](18, 4) NULL,
	[Imp_vAFRMM] [numeric](18, 4) NULL,
	[Imp_vDirAntiDump] [numeric](18, 4) NULL,
	[Imp_vOutDsp] [numeric](18, 4) NULL,
	[Imp_vTotDespes] [numeric](18, 4) NULL,
	[Vlr_Bonificacao] [numeric](18, 4) NULL,
	[Vlr_DscBonRat] [numeric](18, 4) NULL,
 CONSTRAINT [PK_R_NFEIT] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Protocolo] ASC,
	[Cod_Produto] ASC,
	[Cod_Lote] ASC,
	[Num_SeqIte] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Num_SeqIte]  DEFAULT (0) FOR [Num_SeqIte]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_Pedido]  DEFAULT (0) FOR [Qtd_Pedido]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_Bonificacao]  DEFAULT (0) FOR [Qtd_Bonificacao]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_PedFat]  DEFAULT (0) FOR [Qtd_PedFat]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_BonFat]  DEFAULT (0) FOR [Qtd_BonFat]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Flg_ComisNormal]  DEFAULT (0) FOR [Flg_ComisNormal]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_UniFat]  DEFAULT (0) FOR [Prc_UniFat]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_Unitario]  DEFAULT (0) FOR [Prc_Unitario]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_DescItem]  DEFAULT (0) FOR [Per_DescItem]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasIcmsNor]  DEFAULT (0) FOR [Vlr_BasIcmsNor]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_Icms]  DEFAULT (0) FOR [Alq_Icms]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_IcmsNor]  DEFAULT (0) FOR [Vlr_IcmsNor]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Ipi]  DEFAULT (0) FOR [Vlr_Ipi]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_IcmsTrib]  DEFAULT (0) FOR [Vlr_IcmsTrib]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_SubsTrib]  DEFAULT (0) FOR [Vlr_SubsTrib]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_AntRec]  DEFAULT (0) FOR [Vlr_AntRec]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DespRateada]  DEFAULT (0) FOR [Vlr_DespRateada]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DescRateado]  DEFAULT (0) FOR [Vlr_DescRateado]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasIpi]  DEFAULT (0) FOR [Vlr_BasIpi]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasIcmsTrib]  DEFAULT (0) FOR [Vlr_BasIcmsTrib]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_PrdSubTri]  DEFAULT (0) FOR [Vlr_PrdSubTri]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasSubsTrib]  DEFAULT (0) FOR [Vlr_BasSubsTrib]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_PrdAntRec]  DEFAULT (0) FOR [Vlr_PrdAntRec]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasAntRec]  DEFAULT (0) FOR [Vlr_BasAntRec]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Isent]  DEFAULT (0) FOR [Vlr_Isento]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_LiqIte]  DEFAULT (0) FOR [Vlr_LiqIte]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Comissao]  DEFAULT (0) FOR [Vlr_Comissao]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_ComTlmkt]  DEFAULT (0) FOR [Vlr_ComTlmkt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Cus_LiqUni]  DEFAULT (0) FOR [Cus_LiqUni]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_CustoMedio]  DEFAULT (0) FOR [Prc_CustoMedio]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_Venda]  DEFAULT (0) FOR [Prc_Venda]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_FATOR_FAT]  DEFAULT (0) FOR [FATOR_FAT]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Cod_Cfo]  DEFAULT (0) FOR [Cod_Cfo]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasDifTri]  DEFAULT (0) FOR [Vlr_BasDifTri]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DifTri]  DEFAULT (0) FOR [Vlr_DifTri]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Outros]  DEFAULT (0) FOR [Vlr_Outros]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasRepIcm]  DEFAULT (0) FOR [Vlr_BasRepIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_RepIcm]  DEFAULT (0) FOR [Vlr_RepIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Frete]  DEFAULT (0) FOR [Vlr_Frete]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_Markup]  DEFAULT (0) FOR [Per_Markup]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Val_PrcVen]  DEFAULT (0) FOR [Val_PrcVen]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Flg_DbqPrdBlq]  DEFAULT (0) FOR [Flg_DbqPrdBlq]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_MaxCon]  DEFAULT (0) FOR [Prc_MaxCon]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_Fabric]  DEFAULT (0) FOR [Prc_Fabric]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Num_SeqDig]  DEFAULT (0) FOR [Num_SeqDig]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_Promocao]  DEFAULT (0) FOR [Qtd_Promocao]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_EmbFec]  DEFAULT (0) FOR [Qtd_EmbFec]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DspExtNotFis]  DEFAULT (0) FOR [Vlr_DspExtNotFis]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DscExtNotFis]  DEFAULT (0) FOR [Vlr_DscExtNotFis]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_FisAnt]  DEFAULT (0) FOR [Qtd_FisAnt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_CusMedAnt]  DEFAULT (0) FOR [Prc_CusMedAnt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalIcmDeb]  DEFAULT (0) FOR [Per_RedBasCalIcmDeb]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_AgrDeb]  DEFAULT (0) FOR [Alq_AgrDeb]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalDeb]  DEFAULT (0) FOR [Per_RedBasCalDeb]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IcmDeb]  DEFAULT (0) FOR [Alq_IcmDeb]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DebIcm]  DEFAULT (0) FOR [Vlr_DebIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalIcmCre]  DEFAULT (0) FOR [Per_RedBasCalIcmCre]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_LimBasCalCreSbt]  DEFAULT (0) FOR [Per_LimBasCalCreSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_LimCreIcm]  DEFAULT (0) FOR [Vlr_LimCreIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasTri]  DEFAULT (0) FOR [Vlr_BasTri]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalIcm]  DEFAULT (0) FOR [Per_RedBasCalIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalIcmSbt]  DEFAULT (0) FOR [Per_RedBasCalIcmSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IcmSbt]  DEFAULT (0) FOR [Alq_IcmSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_AgrSbt]  DEFAULT (0) FOR [Alq_AgrSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Seguro]  DEFAULT (0) FOR [Vlr_Seguro]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasPis]  DEFAULT (0) FOR [Vlr_BasPis]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_Pis]  DEFAULT (0) FOR [Alq_Pis]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Pis]  DEFAULT (0) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasCofins]  DEFAULT (0) FOR [Vlr_BasCofins]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_Cofins]  DEFAULT (0) FOR [Alq_Cofins]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Cofins]  DEFAULT (0) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_Ipi]  DEFAULT (0) FOR [Alq_Ipi]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Verba]  DEFAULT (0) FOR [Vlr_Verba]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_PrcBasRecSbt]  DEFAULT (0) FOR [Vlr_PrcBasRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_AgrDebRecSbt]  DEFAULT (0) FOR [Alq_AgrDebRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalIcmDebRecSbt]  DEFAULT (0) FOR [Per_RedBasCalIcmDebRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IcmDebRecSbt]  DEFAULT (0) FOR [Alq_IcmDebRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Flg_RegEspTrb]  DEFAULT (0) FOR [Flg_RegEspTrb]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DspSbt]  DEFAULT (0) FOR [Vlr_DspSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasSbtRes]  DEFAULT ((0)) FOR [Vlr_BasSbtRes]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_SbtRes]  DEFAULT ((0)) FOR [Vlr_SbtRes]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_IpiCad]  DEFAULT ((0)) FOR [Per_IpiCad]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_Suframa]  DEFAULT ((0)) FOR [Vlr_Suframa]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Flg_UsaCreIcmSbt]  DEFAULT ((0)) FOR [Flg_UsaCreIcmSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Cod_Estabe]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_Unitar]  DEFAULT ((0)) FOR [Prc_Unitar]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Prc_RefFpb]  DEFAULT ((0)) FOR [Prc_RefFpb]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_PrcBasDevRecSbt]  DEFAULT ((0)) FOR [Vlr_PrcBasDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_AgrDebDevRecSbt]  DEFAULT ((0)) FOR [Alq_AgrDebDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_RedBasCalIcmDebDevRecSbt]  DEFAULT ((0)) FOR [Per_RedBasCalIcmDebDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IcmDebDevRecSbt]  DEFAULT ((0)) FOR [Alq_IcmDebDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasDevRecSbt]  DEFAULT ((0)) FOR [Vlr_BasDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DevRecSbt]  DEFAULT ((0)) FOR [Vlr_DevRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DscIte1]  DEFAULT ((0)) FOR [Vlr_DscIte1]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DscIte2]  DEFAULT ((0)) FOR [Vlr_DscIte2]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BruItem]  DEFAULT ((0)) FOR [Vlr_BruItem]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_TotItem]  DEFAULT ((0)) FOR [Vlr_TotItem]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DescItem]  DEFAULT ((0)) FOR [Vlr_DescItem]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_LiqCmpIte]  DEFAULT ((0)) FOR [Vlr_LiqCmpIte]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_CrePreIcmSbtExt]  DEFAULT ((0)) FOR [Vlr_CrePreIcmSbtExt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_ResIcmSbtIntSN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_ResIcmSbtExtEN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasCrePreIcmSbtExt]  DEFAULT ((0)) FOR [Vlr_BasCrePreIcmSbtExt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_CrePreIcmSbtExt]  DEFAULT ((0)) FOR [Alq_CrePreIcmSbtExt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasResIcmSbtIntSN]  DEFAULT ((0)) FOR [Vlr_BasResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_ResIcmSbtIntSN]  DEFAULT ((0)) FOR [Alq_ResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasResIcmSbtExtEN]  DEFAULT ((0)) FOR [Vlr_BasResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_ResIcmSbtExtEN]  DEFAULT ((0)) FOR [Alq_ResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_MvaResIcmSbtIntSN]  DEFAULT ((0)) FOR [Alq_MvaResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Flg_BlqDsc]  DEFAULT ((0)) FOR [Flg_BlqDsc]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Num_NfePrcBasRecSbt]  DEFAULT ((0)) FOR [Num_NfePrcBasRecSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_AcrAlqIntFcp]  DEFAULT ((0)) FOR [Per_AcrAlqIntFcp]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_PrvParIcm]  DEFAULT ((0)) FOR [Per_PrvParIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_IcmFcpDes]  DEFAULT ((0)) FOR [Vlr_IcmFcpDes]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_IcmParDes]  DEFAULT ((0)) FOR [Vlr_IcmParDes]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_IcmParRem]  DEFAULT ((0)) FOR [Vlr_IcmParRem]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_ResExt]  DEFAULT ((0)) FOR [Per_ResExt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasDspExt]  DEFAULT ((0)) FOR [Vlr_BasDspExt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DspExt]  DEFAULT ((0)) FOR [Vlr_DspExt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IntIcmDes]  DEFAULT ((0)) FOR [Alq_IntIcmDes]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DspCusEnt]  DEFAULT ((0)) FOR [Vlr_DspCusEnt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_BasCalSubTriEntMedAnt]  DEFAULT ((0)) FOR [Vlr_BasCalSubTriEntMedAnt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IcmPresum]  DEFAULT ((0)) FOR [Alq_IcmPresum]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Flg_UsaCrePresum]  DEFAULT ((0)) FOR [Flg_UsaCrePresum]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_Avaria]  DEFAULT ((0)) FOR [Qtd_Avaria]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Per_DscNegFor]  DEFAULT ((0)) FOR [Per_DscNegFor]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Alq_IcmDif]  DEFAULT ((0)) FOR [Alq_IcmDif]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_IcmsDif]  DEFAULT ((0)) FOR [Vlr_IcmsDif]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmsDeson]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_DscCalSuframa]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Alq_FcpIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbtRec]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRec]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Alq_FcpSbtAnt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtAnt]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Per_IcmDiferi]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmDiferi]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vBCSTRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_pST]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vICMSSubstituto]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vICMSSTRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vBCFCPSTRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_pFCPSTRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vFCPSTRet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_pRedBCEfet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vBCEfet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_pICMSEfet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Xml_vICMSEfet]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_DspDevST]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_PisEstDev]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_CofEstDev]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Vlr_DscTri]  DEFAULT ((0)) FOR [Vlr_DscTri]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_IcmRetXml]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_BasIcmRetXml]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Prc_PonFin]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  CONSTRAINT [DF_R_NFEIT_Qtd_PesLiq]  DEFAULT ((0)) FOR [Qtd_PesLiq]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Per_RepIcm]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_ComSup]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_ComSupOpe]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_ComGer]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_ComGerOpe]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Id_DclImp]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_nAdicao]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_nSeqAdic]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vBCII]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vDespAdu]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vII]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vIOF]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vMLE]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vFrete]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vSeguro]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vMLD]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vSiscomex]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vAFRMM]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vDirAntiDump]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vOutDsp]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Imp_vTotDespes]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_Bonificacao]
GO

ALTER TABLE [dbo].[R_NFEIT] ADD  DEFAULT ((0)) FOR [Vlr_DscBonRat]
GO

ALTER TABLE [dbo].[R_NFEIT]  WITH NOCHECK ADD  CONSTRAINT [FK_R_NFEIT_NFECB] FOREIGN KEY([Cod_Estabe], [Protocolo])
REFERENCES [dbo].[R_NFECB] ([Cod_Estabe], [Protocolo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_NFEIT] CHECK CONSTRAINT [FK_R_NFEIT_NFECB]
GO

ALTER TABLE [dbo].[R_NFEIT]  WITH NOCHECK ADD  CONSTRAINT [FK_R_NFEIT_PRODU] FOREIGN KEY([Cod_Produto])
REFERENCES [dbo].[R_PRODU] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_NFEIT] CHECK CONSTRAINT [FK_R_NFEIT_PRODU]
GO


