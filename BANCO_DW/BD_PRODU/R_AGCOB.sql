USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_AGCOB]    Script Date: 28/06/2024 10:27:20 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_AGCOB](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](25) NULL,
	[Tipo] [varchar](2) NULL,
	[Cod_TipCobDef] [varchar](2) NULL,
	[Cod_Carteira] [int] NULL,
	[Qtd_DiaAtrPer] [int] NULL,
	[Cod_Banco] [int] NULL,
	[Cod_Agencia] [varchar](6) NULL,
	[Num_Conta] [varchar](13) NULL,
	[Cod_Convenio] [varchar](20) NULL,
	[Cod_EmpMae] [varchar](16) NULL,
	[Cod_Usuario] [varchar](10) NULL,
	[Flg_ImpBlo] [bit] NULL,
	[Flg_BloPreImp] [bit] NULL,
	[Flg_ImpDup] [bit] NULL,
	[Des_Impressora] [varchar](25) NULL,
	[Cod_Ctapar] [int] NULL,
	[Cod_LocDevPri] [varchar](3) NULL,
	[Cod_CtaDevPri] [varchar](15) NULL,
	[Cod_LocCrePri] [varchar](3) NULL,
	[Cod_CtaCrePri] [varchar](15) NULL,
	[Cod_HisPri] [varchar](3) NULL,
	[Cod_LocDevJur] [varchar](3) NULL,
	[Cod_CtaDevJur] [varchar](15) NULL,
	[Cod_LocCreJur] [varchar](3) NULL,
	[Cod_CtaCreJur] [varchar](15) NULL,
	[Cod_HisJur] [varchar](3) NULL,
	[Cod_LocDevDsc] [varchar](3) NULL,
	[Cod_CtaDevDsc] [varchar](15) NULL,
	[Cod_LocCreDsc] [varchar](3) NULL,
	[Cod_CtaCreDsc] [varchar](15) NULL,
	[Cod_HisDsc] [varchar](3) NULL,
	[Bloqueado] [bit] NULL,
	[Flg_RegCob] [bit] NULL,
	[Blq_Msg1] [varchar](80) NULL,
	[Blq_Msg2] [varchar](80) NULL,
	[Blq_Obs1] [varchar](50) NULL,
	[Blq_Obs2] [varchar](50) NULL,
	[Blq_Obs3] [varchar](50) NULL,
	[Blq_Obs4] [varchar](50) NULL,
	[Num_UltBlq] [varchar](18) NULL,
	[Num_Contrato] [varchar](20) NULL,
	[Cod_UsoBco] [varchar](20) NULL,
	[Flg_BlqAltPdv] [bit] NULL,
	[Flg_CobCnv] [bit] NULL,
	[Cod_BcoCnv] [int] NULL,
	[Des_Cedente] [varchar](45) NULL,
	[Cgc_Cpf_Cedente] [varchar](14) NULL,
	[Pessoa_Cedente] [char](1) NULL,
	[Flg_Cedente] [bit] NULL,
	[Vlr_DspBolBan] [numeric](18, 4) NULL,
	[Flg_CobDspBolBanNtf] [bit] NULL,
	[Obs1_Nf] [varchar](80) NULL,
	[Per_MsgBlo1] [numeric](18, 4) NULL,
	[Per_MsgBlo2] [numeric](18, 4) NULL,
	[Per_MsgBlo3] [numeric](18, 4) NULL,
	[Cod_Aceite] [char](1) NULL,
	[Isn_CtaFin] [int] NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Cod_VarCarteira] [varchar](4) NULL,
	[Cod_LayCob] [varchar](1) NULL,
	[Num_CarBlt] [int] NULL,
	[Flg_CobEle] [bit] NULL,
	[Qtd_DiaProtes] [int] NULL,
	[Qtd_DiaDevolucao] [int] NULL,
	[Num_UltRem] [int] NULL,
	[Des_PrfArqRem] [varchar](3) NULL,
	[Nom_DirExpArq] [varchar](100) NULL,
	[Flg_DepIde] [bit] NULL,
	[Num_UltRemDepIde] [int] NULL,
	[Flg_Excluido] [bit] NULL,
	[Versao] [bigint] NOT NULL,
	[Cod_EstabeOpe] [int] NULL,
	[Cod_HisDscDev] [varchar](3) NULL,
	[Cod_LocDevDscDev] [varchar](3) NULL,
	[Cod_CtaDevDscDev] [varchar](15) NULL,
	[Cod_LocCreDscDev] [varchar](3) NULL,
	[Cod_CtaCreDscDev] [varchar](15) NULL,
	[Num_CvnPagEle] [varchar](20) NULL,
	[Num_LytPagEle] [int] NULL,
	[Des_ArqPagEle] [varchar](200) NULL,
	[Flg_DscFinDspBan] [bit] NOT NULL,
	[Per_TaxAdm] [numeric](18, 4) NOT NULL,
	[Cod_AplTaxAdm] [varchar](1) NOT NULL,
	[Flg_DebTarCobVlrCre] [bit] NULL,
	[Flg_IntInfBolet] [bit] NULL,
	[Flg_DebTarCobVlrCreCNAB400] [bit] NULL,
	[Flg_IntBanBrasil] [bit] NULL,
	[Cod_CliVinc] [int] NULL,
	[FlgExibLogoBol] [bit] NULL,
	[FlgExibDadEstab] [bit] NULL,
	[Api_CliId] [varchar](500) NULL,
	[Api_CliSec] [varchar](500) NULL,
	[Api_CliIdSec] [varchar](600) NULL,
	[Blq_Obs5] [varchar](450) NULL,
 CONSTRAINT [PK_R_AGCOB] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Qtd_DiaAtrPer]  DEFAULT (0) FOR [Qtd_DiaAtrPer]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_ImpBlo]  DEFAULT (0) FOR [Flg_ImpBlo]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_BloPreImp]  DEFAULT (0) FOR [Flg_BloPreImp]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_ImpDup]  DEFAULT (0) FOR [Flg_ImpDup]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Bloqueado]  DEFAULT (0) FOR [Bloqueado]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_RegCob]  DEFAULT (0) FOR [Flg_RegCob]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_BlqAltPdv]  DEFAULT (0) FOR [Flg_BlqAltPdv]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_CobCnv]  DEFAULT (0) FOR [Flg_CobCnv]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Cod_BcoCnv]  DEFAULT (0) FOR [Cod_BcoCnv]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_Cedente]  DEFAULT (0) FOR [Flg_Cedente]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Vlr_DspBolBan]  DEFAULT (0) FOR [Vlr_DspBolBan]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_CobDspBolBanNtf]  DEFAULT (0) FOR [Flg_CobDspBolBanNtf]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Isn_CtaFin]  DEFAULT ((0)) FOR [Isn_CtaFin]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  DEFAULT ((17)) FOR [Num_CarBlt]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_CobEle]  DEFAULT ((0)) FOR [Flg_CobEle]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Qtd_DiaProtes]  DEFAULT ((0)) FOR [Qtd_DiaProtes]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Qtd_DiaDevolucao]  DEFAULT ((0)) FOR [Qtd_DiaDevolucao]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Num_UltRem]  DEFAULT ((0)) FOR [Num_UltRem]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_DepIde]  DEFAULT ((0)) FOR [Flg_DepIde]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Num_UltRemDepIde]  DEFAULT ((0)) FOR [Num_UltRemDepIde]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_Excluido]  DEFAULT ((0)) FOR [Flg_Excluido]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Versao]  DEFAULT ((0)) FOR [Versao]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Cod_EstabeOpe]  DEFAULT ((0)) FOR [Cod_EstabeOpe]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  DEFAULT ((0)) FOR [Flg_DscFinDspBan]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  DEFAULT ((0)) FOR [Per_TaxAdm]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  DEFAULT ('') FOR [Cod_AplTaxAdm]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_DebTarCobVlrCre]  DEFAULT ((0)) FOR [Flg_DebTarCobVlrCre]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_IntInfBolet]  DEFAULT ((0)) FOR [Flg_IntInfBolet]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_DebTarCobVlrCreCNAB400]  DEFAULT ((0)) FOR [Flg_DebTarCobVlrCreCNAB400]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  CONSTRAINT [DF_R_AGCOB_Flg_IntBanBrasil]  DEFAULT ((0)) FOR [Flg_IntBanBrasil]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  DEFAULT ((0)) FOR [FlgExibLogoBol]
GO

ALTER TABLE [dbo].[R_AGCOB] ADD  DEFAULT ((0)) FOR [FlgExibDadEstab]
GO


