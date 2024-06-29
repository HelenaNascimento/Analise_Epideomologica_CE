USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_PRXES]    Script Date: 28/06/2024 08:53:16 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_PRXES](
	[Cod_Estabe] [int] NOT NULL,
	[Cod_Produt] [int] NOT NULL,
	[_Cod_LocFis] [varchar](20) NULL,
	[_Num_Rua] [smallint] NULL,
	[_Num_Col] [smallint] NULL,
	[_Num_Niv] [smallint] NULL,
	[_Num_Apt] [smallint] NULL,
	[Qtd_Transi] [int] NULL,
	[Qtd_Fisico] [int] NULL,
	[Qtd_Solici] [int] NULL,
	[Qtd_Avaria] [int] NULL,
	[Qtd_Reserv] [int] NULL,
	[Qtd_Dispon] [int] NULL,
	[Qtd_Quaren] [int] NULL,
	[Prc_Venda] [numeric](18, 8) NULL,
	[Dat_PrcAtual] [smalldatetime] NULL,
	[Prc_VenAnt] [numeric](18, 8) NULL,
	[Dat_PrcAnt] [smalldatetime] NULL,
	[Prc_CusLiqEnt] [numeric](18, 8) NULL,
	[Prc_CusLiqEntDep] [numeric](18, 8) NULL,
	[Prc_CusMed] [numeric](18, 8) NULL,
	[Prc_CusMedPra] [numeric](18, 8) NULL,
	[Prc_CusMedDep] [numeric](18, 8) NULL,
	[Prc_CusMedCom] [numeric](18, 8) NULL,
	[Prc_EntAnt] [numeric](18, 8) NULL,
	[Per_DscEntAnt] [numeric](7, 4) NULL,
	[Qtd_EntAnt] [int] NULL,
	[Dat_EntAnt] [smalldatetime] NULL,
	[Prc_UltEnt] [numeric](18, 8) NULL,
	[Per_DscUltEnt] [numeric](7, 4) NULL,
	[Qtd_UltEnt] [int] NULL,
	[Dat_UltCompra] [smalldatetime] NULL,
	[Prc_EntAntDep] [numeric](18, 8) NULL,
	[Per_DscEntAntDep] [numeric](7, 4) NULL,
	[Qtd_EntAntDep] [int] NULL,
	[Dat_EntAntDep] [smalldatetime] NULL,
	[Prc_UltEntDep] [numeric](18, 8) NULL,
	[Per_DscUltEntDep] [numeric](7, 4) NULL,
	[Qtd_UltEntDep] [int] NULL,
	[Dat_UltEntDep] [smalldatetime] NULL,
	[Flg_RegSbtEsp] [bit] NULL,
	[Usuario] [varchar](15) NULL,
	[Transacao] [smalldatetime] NULL,
	[Qtd_PrmSolici] [int] NULL,
	[Qtd_PrmFisico] [int] NULL,
	[Qtd_PrmDispon] [int] NULL,
	[Prc_Fabric] [numeric](18, 8) NULL,
	[Prc_MaxCon] [numeric](18, 8) NULL,
	[Dat_PrcFab] [smalldatetime] NULL,
	[Qtd_EstMin] [int] NULL,
	[Qtd_EstMinCfg] [int] NULL,
	[Qtd_EstMax] [int] NULL,
	[Qtd_EstMaxCfg] [int] NULL,
	[_Qtd_EstMinVrj] [int] NULL,
	[_Qtd_EstMaxVrj] [int] NULL,
	[_Cod_Zon] [smallint] NULL,
	[Per_DscBasComNor] [numeric](18, 4) NULL,
	[Per_ComEnt] [numeric](18, 4) NULL,
	[Per_DscMaxVis] [numeric](18, 4) NULL,
	[Per_DscMaxPrz] [numeric](18, 4) NULL,
	[Qtd_PrzMaxFat] [int] NULL,
	[Per_MarkupCusCom] [numeric](18, 4) NULL,
	[Flg_UsoExcHsp] [bit] NULL,
	[Flg_CesBas] [bit] NULL,
	[Per_DscAut] [numeric](18, 4) NULL,
	[Per_DscAutOrc] [numeric](18, 4) NULL,
	[Per_BonAut] [numeric](18, 4) NULL,
	[Flg_BlqDsc] [bit] NULL,
	[Flg_BlqVen] [bit] NULL,
	[Flg_BlqCmp] [bit] NULL,
	[Flg_BlqInfVen] [bit] NULL,
	[Flg_BlqInfPar] [bit] NULL,
	[Flg_BlqCot] [bit] NULL,
	[Flg_BlqPrp] [bit] NULL,
	[Flg_BlqCfv] [bit] NULL,
	[Prc_Tabela] [numeric](18, 8) NULL,
	[Per_PlaRepIcm] [numeric](18, 8) NULL,
	[Per_PlaDesc1] [numeric](18, 8) NULL,
	[Per_PlaDesc2] [numeric](18, 8) NULL,
	[Per_PlaBonific] [numeric](18, 8) NULL,
	[Per_PlaCreIcm] [numeric](18, 8) NULL,
	[Per_PlaDebIcm] [numeric](18, 8) NULL,
	[Per_PlaRebate] [numeric](18, 8) NULL,
	[Per_PlaAgrega] [numeric](18, 8) NULL,
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
	[Taxa_PlaFinanc] [numeric](18, 8) NULL,
	[Prc_CusLiqPla] [numeric](18, 8) NULL,
	[Prc_Pmz] [numeric](18, 8) NULL,
	[Per_PlaMarOpe] [numeric](18, 8) NULL,
	[Per_PlaMarFin] [numeric](18, 8) NULL,
	[Vlr_PlaPrcVen] [numeric](18, 8) NULL,
	[Dias_PlaFinanc] [int] NULL,
	[Flg_PlaFatPrcLiq] [bit] NULL,
	[Sta_AbcAcesso] [varchar](1) NULL,
	[Sta_AbcUniVen] [varchar](1) NULL,
	[Sta_AbcValFat] [varchar](1) NULL,
	[Flg_Bloqueado] [bit] NULL,
	[Dat_PrxVctLot] [smalldatetime] NULL,
	[Cod_Promocao] [int] NULL,
	[Cod_OriMer] [varchar](1) NULL,
	[Cod_ClaTri] [varchar](4) NULL,
	[Per_ParticFat] [numeric](18, 8) NULL,
	[Vlr_BasCalSubTriEntMed] [numeric](18, 4) NULL,
	[Qtd_Pulmao] [int] NULL,
	[Tip_Rentab] [varchar](1) NULL,
	[Flg_CusMedComRentab] [bit] NOT NULL,
	[Per_Rentab] [numeric](18, 4) NOT NULL,
	[Vlr_Rentab] [numeric](18, 4) NOT NULL,
	[Qtd_EstTraMan] [int] NULL,
	[Qtd_DiaSupPrd] [int] NULL,
	[Qtd_CanCompra] [int] NULL,
	[Dat_PrxVctLtl] [datetime] NULL,
	[Sta_AbcUniVenFab] [varchar](1) NULL,
	[Sta_AbcValFatFab] [varchar](1) NULL,
	[Per_LucMin] [numeric](18, 4) NULL,
	[Tip_BlqLuc] [varchar](1) NULL,
	[Qtd_CngCli] [int] NULL,
	[Qtd_CrossDock] [int] NULL,
	[Dat_UltVen] [datetime] NULL,
	[Qtd_MesAleRec] [int] NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Alq_Ipi] [numeric](7, 4) NULL,
	[Flg_UsaFatCxaDun14] [bit] NULL,
	[Cod_Antigo] [int] NULL,
 CONSTRAINT [PK_R_PRXES] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Cod_Produt] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Num_Rua]  DEFAULT ((0)) FOR [_Num_Rua]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Num_Col]  DEFAULT ((0)) FOR [_Num_Col]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Num_Niv]  DEFAULT ((0)) FOR [_Num_Niv]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Num_Apt]  DEFAULT ((0)) FOR [_Num_Apt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Transi]  DEFAULT ((0)) FOR [Qtd_Transi]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Fisico]  DEFAULT ((0)) FOR [Qtd_Fisico]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Solici]  DEFAULT ((0)) FOR [Qtd_Solici]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Avaria]  DEFAULT ((0)) FOR [Qtd_Avaria]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Reserv]  DEFAULT ((0)) FOR [Qtd_Reserv]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Dispon]  DEFAULT ((0)) FOR [Qtd_Dispon]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Quaren]  DEFAULT ((0)) FOR [Qtd_Quaren]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_Venda]  DEFAULT ((0)) FOR [Prc_Venda]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_VenAnt]  DEFAULT ((0)) FOR [Prc_VenAnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusLiqEnt]  DEFAULT ((0)) FOR [Prc_CusLiqEnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusLiqEntDep]  DEFAULT ((0)) FOR [Prc_CusLiqEntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusMed]  DEFAULT ((0)) FOR [Prc_CusMed]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusMedPra]  DEFAULT ((0)) FOR [Prc_CusMedPra]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusMedDep]  DEFAULT ((0)) FOR [Prc_CusMedDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusMedCom]  DEFAULT ((0)) FOR [Prc_CusMedCom]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_EntAnt]  DEFAULT ((0)) FOR [Prc_EntAnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscEntAnt]  DEFAULT ((0)) FOR [Per_DscEntAnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EntAnt]  DEFAULT ((0)) FOR [Qtd_EntAnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_UltEnt]  DEFAULT ((0)) FOR [Prc_UltEnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscUltEnt]  DEFAULT ((0)) FOR [Per_DscUltEnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_UltEnt]  DEFAULT ((0)) FOR [Qtd_UltEnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_EntAntDep]  DEFAULT ((0)) FOR [Prc_EntAntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscEntAntDep]  DEFAULT ((0)) FOR [Per_DscEntAntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EntAntDep]  DEFAULT ((0)) FOR [Qtd_EntAntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_UltEntDep]  DEFAULT ((0)) FOR [Prc_UltEntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscUltEntDep]  DEFAULT ((0)) FOR [Per_DscUltEntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_UltEntDep]  DEFAULT ((0)) FOR [Qtd_UltEntDep]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_RegSbtEsp]  DEFAULT ((0)) FOR [Flg_RegSbtEsp]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_PrmSolici]  DEFAULT ((0)) FOR [Qtd_PrmSolici]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_PrmFisico]  DEFAULT ((0)) FOR [Qtd_PrmFisico]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_PrmDispon]  DEFAULT ((0)) FOR [Qtd_PrmDispon]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_Fabric]  DEFAULT ((0)) FOR [Prc_Fabric]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_MaxCon]  DEFAULT ((0)) FOR [Prc_MaxCon]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstMin]  DEFAULT ((0)) FOR [Qtd_EstMin]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstMinCfg]  DEFAULT ((0)) FOR [Qtd_EstMinCfg]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstMax]  DEFAULT ((0)) FOR [Qtd_EstMax]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstMaxCfg]  DEFAULT ((0)) FOR [Qtd_EstMaxCfg]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstMinVrj]  DEFAULT ((0)) FOR [_Qtd_EstMinVrj]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstMaxVrj]  DEFAULT ((0)) FOR [_Qtd_EstMaxVrj]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Cod_Zon]  DEFAULT ((0)) FOR [_Cod_Zon]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscBasComNor]  DEFAULT ((0)) FOR [Per_DscBasComNor]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_ComEnt]  DEFAULT ((0)) FOR [Per_ComEnt]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscMaxVis]  DEFAULT ((0)) FOR [Per_DscMaxVis]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscMaxPrz]  DEFAULT ((0)) FOR [Per_DscMaxPrz]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_PrzMaxFat]  DEFAULT ((0)) FOR [Qtd_PrzMaxFat]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_MarkupCusCom]  DEFAULT ((0)) FOR [Per_MarkupCusCom]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_UsoExcHsp]  DEFAULT ((0)) FOR [Flg_UsoExcHsp]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_CesBas]  DEFAULT ((0)) FOR [Flg_CesBas]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscAut]  DEFAULT ((0)) FOR [Per_DscAut]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_DscAutOrc]  DEFAULT ((0)) FOR [Per_DscAutOrc]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_BonAut]  DEFAULT ((0)) FOR [Per_BonAut]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqDsc]  DEFAULT ((0)) FOR [Flg_BlqDsc]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqVen]  DEFAULT ((0)) FOR [Flg_BlqVen]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqCmp]  DEFAULT ((0)) FOR [Flg_BlqCmp]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqInfVen]  DEFAULT ((0)) FOR [Flg_BlqInfVen]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqInfPar]  DEFAULT ((0)) FOR [Flg_BlqInfPar]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqCot]  DEFAULT ((0)) FOR [Flg_BlqCot]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqPrp]  DEFAULT ((0)) FOR [Flg_BlqPrp]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_BlqCfv]  DEFAULT ((0)) FOR [Flg_BlqCfv]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_Tabela]  DEFAULT ((0)) FOR [Prc_Tabela]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaRepIcm]  DEFAULT ((0)) FOR [Per_PlaRepIcm]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesc1]  DEFAULT ((0)) FOR [Per_PlaDesc1]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesc2]  DEFAULT ((0)) FOR [Per_PlaDesc2]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaBonific]  DEFAULT ((0)) FOR [Per_PlaBonific]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaCreIcm]  DEFAULT ((0)) FOR [Per_PlaCreIcm]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDebIcm]  DEFAULT ((0)) FOR [Per_PlaDebIcm]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaRebate]  DEFAULT ((0)) FOR [Per_PlaRebate]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaAgrega]  DEFAULT ((0)) FOR [Per_PlaAgrega]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaIpi]  DEFAULT ((0)) FOR [Per_PlaIpi]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesFin]  DEFAULT ((0)) FOR [Per_PlaDesFin]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaCusFre]  DEFAULT ((0)) FOR [Per_PlaCusFre]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesOpe]  DEFAULT ((0)) FOR [Per_PlaDesOpe]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesFre]  DEFAULT ((0)) FOR [Per_PlaDesFre]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesCom]  DEFAULT ((0)) FOR [Per_PlaDesCom]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesPis]  DEFAULT ((0)) FOR [Per_PlaDesPis]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesCof]  DEFAULT ((0)) FOR [Per_PlaDesCof]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesIrpj]  DEFAULT ((0)) FOR [Per_PlaDesIrpj]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesConSoc]  DEFAULT ((0)) FOR [Per_PlaDesConSoc]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaDesIcms]  DEFAULT ((0)) FOR [Per_PlaDesIcms]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaMarRes]  DEFAULT ((0)) FOR [Per_PlaMarRes]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Taxa_PlaFinanc]  DEFAULT ((0)) FOR [Taxa_PlaFinanc]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_CusLiqPla]  DEFAULT ((0)) FOR [Prc_CusLiqPla]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Prc_Pmz]  DEFAULT ((0)) FOR [Prc_Pmz]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaMarOpe]  DEFAULT ((0)) FOR [Per_PlaMarOpe]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_PlaMarFin]  DEFAULT ((0)) FOR [Per_PlaMarFin]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Vlr_PlaPrcVen]  DEFAULT ((0)) FOR [Vlr_PlaPrcVen]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Dias_PlaFinanc]  DEFAULT ((0)) FOR [Dias_PlaFinanc]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_PlaFatPrcLiq]  DEFAULT ((0)) FOR [Flg_PlaFatPrcLiq]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Flg_Bloqueado]  DEFAULT ((0)) FOR [Flg_Bloqueado]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Cod_Promocao]  DEFAULT ((0)) FOR [Cod_Promocao]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_ParticFat]  DEFAULT ((0)) FOR [Per_ParticFat]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Vlr_BasCalSubTriEntMed]  DEFAULT ((0)) FOR [Vlr_BasCalSubTriEntMed]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_Pulmao]  DEFAULT ((0)) FOR [Qtd_Pulmao]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT ((0)) FOR [Flg_CusMedComRentab]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT ((0)) FOR [Per_Rentab]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT ((0)) FOR [Vlr_Rentab]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_EstTraMan]  DEFAULT ((0)) FOR [Qtd_EstTraMan]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_DiaSupPrd]  DEFAULT ((0)) FOR [Qtd_DiaSupPrd]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_CanCompra]  DEFAULT ((0)) FOR [Qtd_CanCompra]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Per_LucMin]  DEFAULT ((0)) FOR [Per_LucMin]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_CngCli]  DEFAULT ((0)) FOR [Qtd_CngCli]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT ((0)) FOR [Qtd_CrossDock]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  CONSTRAINT [DF_R_PRXES_Qtd_MesAleRec]  DEFAULT ((0)) FOR [Qtd_MesAleRec]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT ((0)) FOR [Alq_Ipi]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT ((0)) FOR [Flg_UsaFatCxaDun14]
GO

ALTER TABLE [dbo].[R_PRXES] ADD  DEFAULT (NULL) FOR [Cod_Antigo]
GO

ALTER TABLE [dbo].[R_PRXES]  WITH NOCHECK ADD  CONSTRAINT [FK_R_PRXES_R_PRODU] FOREIGN KEY([Cod_Produt])
REFERENCES [dbo].[R_PRODU] ([Codigo])
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_PRXES] CHECK CONSTRAINT [FK_R_PRXES_R_PRODU]
GO


