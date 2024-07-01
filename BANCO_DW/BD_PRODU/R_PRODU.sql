USE BD_DW
GO

/****** Object:  Table [dbo].[R_PRODU]    Script Date: 28/06/2024 08:54:06 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_PRODU](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](80) NULL,
	[Des_Resumi] [varchar](24) NULL,
	[Ref_Fabricante] [varchar](15) NULL,
	[Cod_EAN] [varchar](13) NULL,
	[Peso] [numeric](18, 8) NULL,
	[Unidade_Venda] [varchar](3) NULL,
	[Qtd_Embalagem] [int] NULL,
	[Cod_AbcFar] [int] NULL,
	[Validade] [int] NULL,
	[Cod_PrdExt] [varchar](13) NULL,
	[Localizacao] [varchar](8) NULL,
	[Dat_Cadastro] [smalldatetime] NULL,
	[Dat_UltVenda] [smalldatetime] NULL,
	[Cod_Fabricante] [int] NULL,
	[Cod_Divisao] [int] NULL,
	[Cod_Classif] [varchar](7) NULL,
	[Cod_SubBas] [int] NULL,
	[Sta_AbcValFat] [char](1) NULL,
	[Sta_AbcUniVen] [char](1) NULL,
	[Per_ParticFat] [numeric](18, 8) NULL,
	[Flag_ImprClassif1] [char](1) NULL,
	[Ctrl_Preco] [char](1) NULL,
	[Ctrl_Venda] [char](1) NULL,
	[Cod_GrpPrc] [char](1) NULL,
	[Tip_Por344] [varchar](2) NULL,
	[Tip_LisPis] [char](1) NULL,
	[Cod_ClaFis] [varchar](20) NULL,
	[Prc_Tabela] [numeric](18, 4) NULL,
	[Prc_CusLiqPla] [numeric](18, 4) NULL,
	[Prc_Pmz] [numeric](18, 4) NULL,
	[Cod_Promocao] [int] NULL,
	[Cod_PlaRegTri] [int] NULL,
	[Per_PlaRedCreIcm] [numeric](18, 8) NULL,
	[Per_PlaRepIcm] [numeric](18, 8) NULL,
	[Per_PlaCreIcm] [numeric](18, 8) NULL,
	[Per_PlaDebIcm] [numeric](18, 8) NULL,
	[Per_PlaRebate] [numeric](18, 8) NULL,
	[Per_PlaAgrega] [numeric](18, 8) NULL,
	[Per_PlaDesc1] [numeric](18, 8) NULL,
	[Per_PlaDesc2] [numeric](18, 8) NULL,
	[Per_PlaBonific] [numeric](18, 8) NULL,
	[Flg_PlaFatPrcLiq] [bit] NULL,
	[Per_PlaIpi] [numeric](18, 8) NULL,
	[Per_PlaDesFin] [numeric](18, 8) NULL,
	[Per_PlaCusFre] [numeric](18, 8) NULL,
	[Per_PlaDesOpe] [numeric](18, 8) NULL,
	[Per_PlaDesFre] [numeric](18, 8) NULL,
	[Per_PlaDesCom] [numeric](18, 8) NULL,
	[Per_PlaDesPis] [numeric](18, 8) NULL,
	[Per_PlaDesCof] [numeric](18, 8) NULL,
	[Per_PlaDesIrpj] [numeric](18, 8) NULL,
	[Per_PlaDesConSoc] [numeric](18, 8) NULL,
	[Per_PlaDesIcms] [numeric](18, 8) NULL,
	[Per_PlaMarRes] [numeric](18, 8) NULL,
	[Per_PlaMarOpe] [numeric](18, 8) NULL,
	[Per_PlaMarFin] [numeric](18, 8) NULL,
	[Vlr_PlaPrcVen] [numeric](18, 4) NULL,
	[Dias_PlaFinanc] [int] NULL,
	[Taxa_PlaFinanc] [numeric](18, 8) NULL,
	[Des_PrdDet] [text] NULL,
	[Reg_MS] [varchar](20) NULL,
	[H_DatPrcAtu] [smalldatetime] NULL,
	[H_PrcVen] [numeric](18, 4) NULL,
	[H_PrcVen1] [numeric](18, 4) NULL,
	[H_PrcVen2] [numeric](18, 4) NULL,
	[H_PrcVen3] [numeric](18, 4) NULL,
	[H_DatPrcAnt] [smalldatetime] NULL,
	[H_PrcVen1Ant] [numeric](18, 4) NULL,
	[H_PrcVen2Ant] [numeric](18, 4) NULL,
	[H_PrcVen3Ant] [numeric](18, 4) NULL,
	[H_PrcTab] [numeric](18, 4) NULL,
	[H_PerRepIcm] [numeric](18, 8) NULL,
	[H_PerDsc1] [numeric](18, 8) NULL,
	[H_PerDsc2] [numeric](18, 8) NULL,
	[H_PerOutDsc] [numeric](18, 8) NULL,
	[H_PerCreIcm] [numeric](18, 8) NULL,
	[H_PerRedBasCalDeb] [numeric](18, 8) NULL,
	[H_PerAgrDebIcm] [numeric](18, 8) NULL,
	[H_PerDebIcm] [numeric](18, 8) NULL,
	[H_PerIpi] [numeric](18, 8) NULL,
	[H_PerFrete] [numeric](18, 8) NULL,
	[H_PerDspFin] [numeric](18, 8) NULL,
	[H_PerOutDsp] [numeric](18, 8) NULL,
	[H_PrcCusLiqEnt] [numeric](18, 4) NULL,
	[H_PerMrg1] [numeric](18, 8) NULL,
	[H_PerMrg2] [numeric](18, 8) NULL,
	[H_PerMrg3] [numeric](18, 8) NULL,
	[H_PerRedBasCalCre] [numeric](18, 8) NULL,
	[Dat_PrcUltBal] [smalldatetime] NULL,
	[Per_ComVnd] [numeric](18, 8) NULL,
	[Qtd_SldAntBal] [int] NULL,
	[Qtd_SldPosBal] [int] NULL,
	[Cod_SubBas2] [int] NULL,
	[Cod_SubBas3] [int] NULL,
	[Cod_SubBas4] [int] NULL,
	[Qtd_UndVen] [int] NULL,
	[Des_FormaFarmac] [varchar](12) NULL,
	[Des_NomGen] [varchar](30) NULL,
	[Des_UndVen] [varchar](12) NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[Mrg_PrcVenConFin1] [numeric](18, 8) NULL,
	[Mrg_PrcVenConFin2] [numeric](18, 8) NULL,
	[Mrg_PrcVenConFin3] [numeric](18, 8) NULL,
	[Qtd_FraVen] [int] NULL,
	[Per_Markup] [numeric](18, 8) NULL,
	[Prc_FabAnt] [numeric](18, 4) NULL,
	[Dat_PrcFabAnt] [smalldatetime] NULL,
	[Prc_MaxConAnt] [numeric](18, 4) NULL,
	[Cod_CtrUsu] [varchar](3) NULL,
	[Des_PrdRef] [varchar](20) NULL,
	[Edi_Pra] [int] NULL,
	[Edi_Dep] [int] NULL,
	[Per_BonAutOrc] [numeric](18, 8) NULL,
	[Prc_Minimo] [int] NULL,
	[Descri] [varchar](64) NULL,
	[Aprese] [varchar](15) NULL,
	[Cod_RefPrati] [varchar](14) NULL,
	[Lrg_Emb] [numeric](6, 2) NULL,
	[Alt_Emb] [numeric](6, 2) NULL,
	[Prf_Emb] [numeric](6, 2) NULL,
	[Vol_Emb] [numeric](18, 4) NULL,
	[Flg_PolComercMax] [bit] NULL,
	[Flg_PrzComercMax] [bit] NULL,
	[Und_EmbCmp] [varchar](3) NULL,
	[Lrg_EmbCmp] [numeric](18, 4) NULL,
	[Alt_EmbCmp] [numeric](18, 4) NULL,
	[Prf_EmbCmp] [numeric](18, 4) NULL,
	[Pes_EmbCmp] [numeric](18, 4) NULL,
	[Qtd_EmbPalete] [int] NULL,
	[Qtd_CamPalete] [int] NULL,
	[Cod_EanEmbCmp] [varchar](14) NULL,
	[Flg_IncFis] [bit] NULL,
	[Tipo] [varchar](2) NULL,
	[Tip_Prd] [char](1) NULL,
	[Sub_TipPrd] [char](1) NULL,
	[Dat_VctRegMS] [smalldatetime] NULL,
	[Flg_Servic] [bit] NULL,
	[Flg_ExpWeb] [bit] NULL,
	[Des_NomImgWeb] [varchar](255) NULL,
	[Des_InfImgWeb] [varchar](255) NULL,
	[Per_Ipi] [numeric](18, 4) NULL,
	[Prc_RefFpb] [numeric](18, 4) NULL,
	[Prc_RefFpbAnt] [numeric](18, 4) NULL,
	[Dat_PrcRefFpb] [smalldatetime] NULL,
	[Dat_PrcRefFpbAnt] [smalldatetime] NULL,
	[Prc_Fabric12] [numeric](18, 4) NULL,
	[Prc_MaxCon12] [numeric](18, 4) NULL,
	[Prc_Fabric17] [numeric](18, 4) NULL,
	[Prc_MaxCon17] [numeric](18, 4) NULL,
	[Prc_Fabric18] [numeric](18, 4) NULL,
	[Prc_MaxCon18] [numeric](18, 4) NULL,
	[Prc_Fabric19] [numeric](18, 4) NULL,
	[Prc_MaxCon19] [numeric](18, 4) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Sta_AbcAcesso] [varchar](1) NULL,
	[Num_SeqBal] [int] NULL,
	[Qtd_SldPraAntBal] [int] NULL,
	[Qtd_SldPraPosBal] [int] NULL,
	[Cod_FCI] [varchar](36) NULL,
	[Per_FCI] [numeric](18, 4) NULL,
	[Cod_NcmExt] [varchar](3) NULL,
	[Flg_Generico] [bit] NULL,
	[Cod_GuiFar] [int] NULL,
	[Flg_PesoKg] [bit] NULL,
	[Cod_CEST] [varchar](7) NULL,
	[Prc_Fabric175] [numeric](18, 4) NULL,
	[Prc_MaxCon175] [numeric](18, 4) NULL,
	[Prc_Fabric20] [numeric](18, 4) NULL,
	[Prc_MaxCon20] [numeric](18, 4) NULL,
	[Des_MtvIseRegMS] [varchar](300) NULL,
	[Ref_Detalhe] [varchar](50) NULL,
	[Sta_AbcUniVenFab] [varchar](1) NULL,
	[Sta_AbcValFatFab] [varchar](1) NULL,
	[Flg_ImpEtqGon] [bit] NULL,
	[Cod_GrpDesPrd] [int] NULL,
	[Prc_Fabric21] [numeric](18, 4) NULL,
	[Prc_MaxCon21] [numeric](18, 4) NULL,
	[Prc_Fabric22] [numeric](18, 4) NULL,
	[Prc_MaxCon22] [numeric](18, 4) NULL,
	[Prc_Fabric195] [numeric](18, 4) NULL,
	[Prc_MaxCon195] [numeric](18, 4) NULL,
	[Prc_Fabric205] [numeric](18, 4) NULL,
	[Prc_MaxCon205] [numeric](18, 4) NULL,
 CONSTRAINT [PK_R_PRODU] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Peso]  DEFAULT (0) FOR [Peso]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_Embalagem]  DEFAULT (0) FOR [Qtd_Embalagem]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Cod_AbcFar]  DEFAULT (0) FOR [Cod_AbcFar]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Validade]  DEFAULT (0) FOR [Validade]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_ParticFat]  DEFAULT (0) FOR [Per_ParticFat]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Tabela]  DEFAULT (0) FOR [Prc_Tabela]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_CusLiqPla]  DEFAULT (0) FOR [Prc_CusLiqPla]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Pmz]  DEFAULT (0) FOR [Prc_Pmz]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Cod_Promocao]  DEFAULT (0) FOR [Cod_Promocao]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Cod_PlaRegTri]  DEFAULT (0) FOR [Cod_PlaRegTri]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaRedCreIcm]  DEFAULT (0) FOR [Per_PlaRedCreIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaRepIcm]  DEFAULT (0) FOR [Per_PlaRepIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaCreIcm]  DEFAULT (0) FOR [Per_PlaCreIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDebIcm]  DEFAULT (0) FOR [Per_PlaDebIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaRebate]  DEFAULT (0) FOR [Per_PlaRebate]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaAgrega]  DEFAULT (0) FOR [Per_PlaAgrega]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesc1]  DEFAULT (0) FOR [Per_PlaDesc1]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesc2]  DEFAULT (0) FOR [Per_PlaDesc2]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaBonific]  DEFAULT (0) FOR [Per_PlaBonific]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_PlaFatPrcLiq]  DEFAULT (0) FOR [Flg_PlaFatPrcLiq]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaIpi]  DEFAULT (0) FOR [Per_PlaIpi]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesFin]  DEFAULT (0) FOR [Per_PlaDesFin]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaCusFre]  DEFAULT (0) FOR [Per_PlaCusFre]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesOpe]  DEFAULT (0) FOR [Per_PlaDesOpe]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesFre]  DEFAULT (0) FOR [Per_PlaDesFre]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesCom]  DEFAULT (0) FOR [Per_PlaDesCom]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesPis]  DEFAULT (0) FOR [Per_PlaDesPis]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesCof]  DEFAULT (0) FOR [Per_PlaDesCof]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesIrpj]  DEFAULT (0) FOR [Per_PlaDesIrpj]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesConSoc]  DEFAULT (0) FOR [Per_PlaDesConSoc]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaDesIcms]  DEFAULT (0) FOR [Per_PlaDesIcms]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaMarRes]  DEFAULT (0) FOR [Per_PlaMarRes]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaMarOpe]  DEFAULT (0) FOR [Per_PlaMarOpe]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_PlaMarFin]  DEFAULT (0) FOR [Per_PlaMarFin]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Vlr_PlaPrcVen]  DEFAULT (0) FOR [Vlr_PlaPrcVen]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Dias_PlaFinanc]  DEFAULT (0) FOR [Dias_PlaFinanc]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Taxa_PlaFinanc]  DEFAULT (0) FOR [Taxa_PlaFinanc]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen]  DEFAULT (0) FOR [H_PrcVen]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen1]  DEFAULT (0) FOR [H_PrcVen1]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen2]  DEFAULT (0) FOR [H_PrcVen2]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen3]  DEFAULT (0) FOR [H_PrcVen3]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen1Ant]  DEFAULT (0) FOR [H_PrcVen1Ant]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen2Ant]  DEFAULT (0) FOR [H_PrcVen2Ant]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcVen3Ant]  DEFAULT (0) FOR [H_PrcVen3Ant]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcTab]  DEFAULT (0) FOR [H_PrcTab]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerRepIcm]  DEFAULT (0) FOR [H_PerRepIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerDsc1]  DEFAULT (0) FOR [H_PerDsc1]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerDsc2]  DEFAULT (0) FOR [H_PerDsc2]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerOutDsc]  DEFAULT (0) FOR [H_PerOutDsc]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerCreIcm]  DEFAULT (0) FOR [H_PerCreIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerRedBasCalDeb]  DEFAULT (0) FOR [H_PerRedBasCalDeb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerAgrDebIcm]  DEFAULT (0) FOR [H_PerAgrDebIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerDebIcm]  DEFAULT (0) FOR [H_PerDebIcm]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerIpi]  DEFAULT (0) FOR [H_PerIpi]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerFrete]  DEFAULT (0) FOR [H_PerFrete]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerDspFin]  DEFAULT (0) FOR [H_PerDspFin]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerOutDsp]  DEFAULT (0) FOR [H_PerOutDsp]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PrcCusLiqEnt]  DEFAULT (0) FOR [H_PrcCusLiqEnt]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerMrg1]  DEFAULT (0) FOR [H_PerMrg1]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerMrg2]  DEFAULT (0) FOR [H_PerMrg2]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerMrg3]  DEFAULT (0) FOR [H_PerMrg3]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_H_PerRedBasCalCre]  DEFAULT (0) FOR [H_PerRedBasCalCre]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_ComVnd]  DEFAULT (0) FOR [Per_ComVnd]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_SldAntBal]  DEFAULT (0) FOR [Qtd_SldAntBal]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_SldPosBal]  DEFAULT (0) FOR [Qtd_SldPosBal]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Mrg_PrcVenConFin1]  DEFAULT (0) FOR [Mrg_PrcVenConFin1]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Mrg_PrcVenConFin2]  DEFAULT (0) FOR [Mrg_PrcVenConFin2]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Mrg_PrcVenConFin3]  DEFAULT (0) FOR [Mrg_PrcVenConFin3]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_Markup]  DEFAULT (0) FOR [Per_Markup]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_FabAnt]  DEFAULT (0) FOR [Prc_FabAnt]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxConAnt]  DEFAULT (0) FOR [Prc_MaxConAnt]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Edi_Pra]  DEFAULT (0) FOR [Edi_Pra]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Edi_Dep]  DEFAULT (0) FOR [Edi_Dep]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_BonAutOrc]  DEFAULT (0) FOR [Per_BonAutOrc]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Minimo]  DEFAULT (0) FOR [Prc_Minimo]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Lrg_Emb]  DEFAULT (0) FOR [Lrg_Emb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Alt_Emb]  DEFAULT (0) FOR [Alt_Emb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prf_Emb]  DEFAULT (0) FOR [Prf_Emb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Vol_Emb]  DEFAULT (0) FOR [Vol_Emb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_PolComercMax]  DEFAULT (0) FOR [Flg_PolComercMax]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_PrzComercMax]  DEFAULT (0) FOR [Flg_PrzComercMax]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Lrg_EmbCmp]  DEFAULT (0) FOR [Lrg_EmbCmp]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Alt_EmbCmp]  DEFAULT (0) FOR [Alt_EmbCmp]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prf_EmbCmp]  DEFAULT (0) FOR [Prf_EmbCmp]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Pes_EmbCmp]  DEFAULT (0) FOR [Pes_EmbCmp]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_EmbPalete]  DEFAULT (0) FOR [Qtd_EmbPalete]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_CamPalete]  DEFAULT (0) FOR [Qtd_CamPalete]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_IncFis]  DEFAULT (0) FOR [Flg_IncFis]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_Servic]  DEFAULT (0) FOR [Flg_Servic]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_ExpWeb]  DEFAULT (0) FOR [Flg_ExpWeb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_Ipi]  DEFAULT ((0)) FOR [Per_Ipi]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_RefFpb]  DEFAULT ((0)) FOR [Prc_RefFpb]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_RefFpbAnt]  DEFAULT ((0)) FOR [Prc_RefFpbAnt]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric12]  DEFAULT ((0)) FOR [Prc_Fabric12]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon12]  DEFAULT ((0)) FOR [Prc_MaxCon12]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric17]  DEFAULT ((0)) FOR [Prc_Fabric17]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon17]  DEFAULT ((0)) FOR [Prc_MaxCon17]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric18]  DEFAULT ((0)) FOR [Prc_Fabric18]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon18]  DEFAULT ((0)) FOR [Prc_MaxCon18]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric19]  DEFAULT ((0)) FOR [Prc_Fabric19]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon19]  DEFAULT ((0)) FOR [Prc_MaxCon19]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Num_SeqBal]  DEFAULT ((0)) FOR [Num_SeqBal]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_SldPraAntBal]  DEFAULT ((0)) FOR [Qtd_SldPraAntBal]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Qtd_SldPraPosBal]  DEFAULT ((0)) FOR [Qtd_SldPraPosBal]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Per_FCI]  DEFAULT ((0)) FOR [Per_FCI]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_Generico]  DEFAULT ((0)) FOR [Flg_Generico]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Cod_GuiFar]  DEFAULT ((0)) FOR [Cod_GuiFar]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_PesoKg]  DEFAULT ((0)) FOR [Flg_PesoKg]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric175]  DEFAULT ((0)) FOR [Prc_Fabric175]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon175]  DEFAULT ((0)) FOR [Prc_MaxCon175]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric20]  DEFAULT ((0)) FOR [Prc_Fabric20]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon20]  DEFAULT ((0)) FOR [Prc_MaxCon20]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Flg_ImpEtqGon]  DEFAULT ((0)) FOR [Flg_ImpEtqGon]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  DEFAULT ((0)) FOR [Prc_Fabric21]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  DEFAULT ((0)) FOR [Prc_MaxCon21]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  DEFAULT ((0)) FOR [Prc_Fabric22]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  DEFAULT ((0)) FOR [Prc_MaxCon22]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric195]  DEFAULT ((0)) FOR [Prc_Fabric195]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon195]  DEFAULT ((0)) FOR [Prc_MaxCon195]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_Fabric205]  DEFAULT ((0)) FOR [Prc_Fabric205]
GO

ALTER TABLE [dbo].[R_PRODU] ADD  CONSTRAINT [DF_R_PRODU_Prc_MaxCon205]  DEFAULT ((0)) FOR [Prc_MaxCon205]
GO

ALTER TABLE [dbo].[R_PRODU]  WITH NOCHECK ADD  CONSTRAINT [FK_R_PRODU_R_CLASS] FOREIGN KEY([Cod_Classif])
REFERENCES [dbo].[R_CLASS] ([Codigo])
GO

ALTER TABLE [dbo].[R_PRODU] CHECK CONSTRAINT [FK_R_PRODU_R_CLASS]
GO

ALTER TABLE [dbo].[R_PRODU]  WITH NOCHECK ADD  CONSTRAINT [FK_R_PRODU_FABRI] FOREIGN KEY([Cod_Fabricante])
REFERENCES [dbo].[R_FABRI] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_PRODU] CHECK CONSTRAINT [FK_R_PRODU_R_FABRI]
GO


