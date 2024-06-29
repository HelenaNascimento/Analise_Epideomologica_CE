USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_NFSCB]    Script Date: 28/06/2024 13:04:19 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_NFSCB](
	[Cod_Estabe] [int] NOT NULL,
	[Ser_Nota] [varchar](3) NOT NULL,
	[Num_Nota] [int] NOT NULL,
	[Esp_Nota] [varchar](3) NULL,
	[Status] [char](1) NULL,
	[Tip_Saida] [char](1) NULL,
	[Tip_Vencimento] [char](1) NULL,
	[Cod_OrigemNfs] [varchar](2) NULL,
	[Qtd_PagNF] [int] NULL,
	[Ser_Selo] [varchar](2) NULL,
	[Num_Selo] [bigint] NULL,
	[Num_Formulario] [int] NULL,
	[Dat_Emissao] [smalldatetime] NULL,
	[Dat_Cancelamento] [smalldatetime] NULL,
	[Cod_Cliente] [int] NULL,
	[Cod_Funcionario] [int] NULL,
	[Cod_Fornecedor] [int] NULL,
	[Consumidor] [varchar](45) NULL,
	[Qtd_PrzMed] [numeric](18, 4) NULL,
	[Qtd_Parcela] [int] NULL,
	[Cod_Cfo1] [int] NULL,
	[Per_Faturado] [int] NULL,
	[Cod_Pedido] [int] NULL,
	[Cod_VendTlmkt] [int] NULL,
	[Cod_Vendedor] [int] NULL,
	[Cod_Transportadora] [int] NULL,
	[Cod_Agente] [int] NULL,
	[Tip_Consumidor] [char](1) NULL,
	[Tip_RetImp] [char](1) NULL,
	[Vlr_BasPar] [numeric](18, 4) NULL,
	[Vlr_BasDscCom] [numeric](18, 4) NULL,
	[Vlr_LiqItens] [numeric](18, 4) NULL,
	[VlrBruItens] [numeric](18, 4) NULL,
	[Vlr_RepIcms] [numeric](18, 4) NULL,
	[Vlr_SubsTrib] [numeric](18, 4) NULL,
	[Per_DescontoCom] [numeric](18, 8) NULL,
	[Per_DescontoFin] [numeric](18, 8) NULL,
	[Vlr_DescontoCom] [numeric](18, 4) NULL,
	[Vlr_DscTri] [numeric](18, 4) NULL,
	[Vlr_TotalNota] [numeric](18, 4) NULL,
	[Vlr_BasIcmsNor] [numeric](18, 4) NULL,
	[Vlr_IcmsNor] [numeric](18, 4) NULL,
	[Vlr_BasIcmsTri] [numeric](18, 4) NULL,
	[Vlr_IcmsTri] [numeric](18, 4) NULL,
	[Vlr_BasRepIcms] [numeric](18, 4) NULL,
	[Vlr_PrdSubTri] [numeric](18, 4) NULL,
	[Vlr_BasSubsTrib] [numeric](18, 4) NULL,
	[Vlr_Ipi] [numeric](18, 4) NULL,
	[Vlr_Isento] [numeric](18, 4) NULL,
	[Cod_VlrFiscal] [char](1) NULL,
	[Flg_IcmsSobreTotal] [bit] NULL,
	[Qtd_Peso] [numeric](18, 4) NULL,
	[Qtd_Volumes] [int] NULL,
	[Qtd_Itens] [int] NULL,
	[Flg_ComisNormal] [bit] NULL,
	[Flg_MovEst] [bit] NULL,
	[Vlr_ComPag] [numeric](18, 4) NULL,
	[Vlr_ComPagTlmkt] [numeric](18, 4) NULL,
	[Per_ComVnd] [numeric](18, 8) NULL,
	[Vlr_ComTlmkt] [numeric](18, 4) NULL,
	[Vlr_Comissao] [numeric](18, 4) NULL,
	[Num_NotSubFat] [int] NULL,
	[Cgc] [varchar](14) NULL,
	[Cgf] [varchar](15) NULL,
	[Endereco] [varchar](50) NULL,
	[Bairro] [varchar](20) NULL,
	[Cep] [varchar](8) NULL,
	[Cidade] [varchar](25) NULL,
	[Estado] [varchar](2) NULL,
	[Cod_Cidade] [int] NULL,
	[Cod_MacroReg] [int] NULL,
	[Cod_MicroReg] [int] NULL,
	[Observacao] [varchar](80) NULL,
	[Usuario] [varchar](15) NULL,
	[Transacao] [smalldatetime] NULL,
	[Str_RelDoc] [varchar](42) NULL,
	[Tip_OutSai] [varchar](1) NULL,
	[Vlr_Outros] [numeric](18, 4) NULL,
	[Vlr_Seguro] [numeric](18, 4) NULL,
	[Vlr_OutDsp] [numeric](18, 4) NULL,
	[Tip_Frete] [char](1) NULL,
	[Vlr_Frete] [numeric](18, 4) NULL,
	[Vlr_BasIpi] [numeric](18, 4) NULL,
	[Cod_Ope] [varchar](3) NULL,
	[Tip_DocRel] [varchar](1) NULL,
	[Via_Transp] [varchar](1) NULL,
	[Flg_RetEnt] [bit] NULL,
	[Dat_RetEnt] [smalldatetime] NULL,
	[Val_ComTra] [numeric](18, 4) NULL,
	[Dat_FecCom] [smalldatetime] NULL,
	[Val_ComPagTra] [numeric](18, 4) NULL,
	[Dat_FecComEnt] [smalldatetime] NULL,
	[Cod_Digitador] [int] NULL,
	[Obs_Rodape] [text] NULL,
	[Obs_Corpo] [text] NULL,
	[Per_DscFinLiq] [numeric](18, 4) NULL,
	[Flg_WMS] [bit] NULL,
	[Cod_CidIbge] [varchar](7) NULL,
	[Cod_ModImpDanfe] [char](2) NULL,
	[Cod_FinEmi] [char](1) NULL,
	[Cod_ModDoc] [char](2) NULL,
	[Cod_ModEmiNfe] [char](1) NULL,
	[Id_Lote] [int] NULL,
	[Chv_Acesso] [varchar](44) NULL,
	[Arquivo] [ntext] NULL,
	[Dat_Proc] [datetime] NULL,
	[Num_Protoc] [bigint] NULL,
	[Num_DigValue] [varchar](28) NULL,
	[Cod_StaResp] [smallint] NULL,
	[Des_Motivo] [varchar](255) NULL,
	[Ret_DhRecbto] [datetime] NULL,
	[Ret_NProt] [varchar](15) NULL,
	[Ret_DigVal] [varchar](28) NULL,
	[Ret_CStat] [smallint] NULL,
	[Ret_XMotivo] [varchar](255) NULL,
	[Tam_Xml] [bigint] NULL,
	[Cod_StaNfe] [char](1) NULL,
	[Dat_Saida] [smalldatetime] NULL,
	[Flg_Email] [bit] NULL,
	[Flg_CST_Pis_ST] [bit] NULL,
	[Flg_CST_Cof_ST] [bit] NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[Flg_Storage] [bit] NULL,
	[Vlr_BasRecSbt] [numeric](18, 4) NULL,
	[Vlr_RecSbt] [numeric](18, 4) NULL,
	[Ide_NotEmp] [varchar](20) NULL,
	[Ide_PedCmp] [varchar](40) NULL,
	[Ide_Contra] [varchar](40) NULL,
	[Qtd_PesBru] [numeric](18, 4) NULL,
	[Vlr_SubsTribEmb] [numeric](18, 4) NULL,
	[Cod_PlaVei] [varchar](10) NULL,
	[Id_PolCom] [int] NULL,
	[Flg_BlqImpVol] [bit] NULL,
	[Des_JusCan] [varchar](255) NULL,
	[Vlr_Verba] [numeric](18, 4) NULL,
	[Cod_FinNfe] [char](1) NULL,
	[UF_PlaVei] [varchar](2) NULL,
	[Vlr_BasRecSbtInt] [numeric](18, 8) NULL,
	[Vlr_RecSbtInt] [numeric](18, 8) NULL,
	[Cod_EndFon] [varchar](20) NULL,
	[Tip_EntSai] [varchar](1) NULL,
	[Tip_Entreg] [varchar](1) NULL,
	[Tip_DevEcf] [varchar](1) NULL,
	[Nom_UsuEsp] [varchar](15) NULL,
	[Num_CarFid] [varchar](25) NULL,
	[Num_Caixa] [smallint] NULL,
	[Num_Turno] [smallint] NULL,
	[Cod_ForPag] [int] NULL,
	[Cod_EntDom] [int] NULL,
	[Cod_Operad] [int] NULL,
	[Cod_Orcame] [int] NULL,
	[Num_Docume] [int] NULL,
	[Cod_MovDev] [int] NULL,
	[Cod_Entreg] [int] NULL,
	[Num_IsnNfs] [int] NULL,
	[Val_Subsid] [numeric](18, 4) NULL,
	[Val_Troca] [numeric](18, 4) NULL,
	[Dat_Retorn] [smalldatetime] NULL,
	[Flg_Retorn] [bit] NULL,
	[Flg_Estorn] [bit] NULL,
	[Flg_Altera] [bit] NULL,
	[Cod_RegTri] [smallint] NULL,
	[Ser_NotSubFat] [varchar](3) NULL,
	[Per_ComOpe] [numeric](18, 8) NULL,
	[Flg_RepNfePrc] [bit] NOT NULL,
	[cMsg] [int] NULL,
	[xMsg] [varchar](1200) NULL,
	[Cod_Contrato] [int] NULL,
	[Num_SequenciaCtr] [int] NULL,
	[Vlr_FrePrv] [numeric](18, 4) NULL,
	[Flg_ExpBomPrc] [bit] NULL,
	[Vlr_BasSbtRes] [numeric](18, 4) NULL,
	[Vlr_SbtRes] [numeric](18, 4) NULL,
	[Per_FrePrv] [numeric](18, 4) NULL,
	[Cod_Gerencia] [int] NULL,
	[Per_ComGer] [numeric](18, 4) NULL,
	[Vlr_ComGer] [numeric](18, 4) NULL,
	[Vlr_ComPagGer] [numeric](18, 4) NULL,
	[Cod_Supervisor] [int] NULL,
	[Per_ComSup] [numeric](18, 4) NULL,
	[Vlr_ComSup] [numeric](18, 4) NULL,
	[Vlr_ComPagSup] [numeric](18, 4) NULL,
	[Cod_GerOpe] [int] NULL,
	[Per_ComGerOpe] [numeric](18, 4) NULL,
	[Vlr_ComGerOpe] [numeric](18, 4) NULL,
	[Vlr_ComPagGerOpe] [numeric](18, 4) NULL,
	[Cod_SupOpe] [int] NULL,
	[Per_ComSupOpe] [numeric](18, 4) NULL,
	[Vlr_ComSupOpe] [numeric](18, 4) NULL,
	[Vlr_ComPagSupOpe] [numeric](18, 4) NULL,
	[Vlr_VrbPar] [numeric](18, 4) NULL,
	[Vlr_VrbBon] [numeric](18, 4) NULL,
	[Des_LayoutPde] [varchar](25) NULL,
	[Des_Convenio] [varchar](30) NULL,
	[Cod_PrjPde] [varchar](12) NULL,
	[Id_Transacao] [int] NULL,
	[Vlr_DscBon] [numeric](18, 4) NULL,
	[Vlr_VrbVdr] [numeric](18, 4) NULL,
	[Vlr_VrbSup] [numeric](18, 4) NULL,
	[Ide_NumPregao] [varchar](20) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Num_SeqBal] [int] NULL,
	[Vlr_ResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_ResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Cod_PedCliPde] [varchar](20) NULL,
	[Cod_DepMovEst] [smallint] NULL,
	[Cod_ZonMovEst] [smallint] NULL,
	[Xml_Cancelamento] [ntext] NULL,
	[Cod_DDD] [varchar](2) NULL,
	[Cod_LayoutPde] [int] NULL,
	[Vlr_IcmFcpDes] [numeric](18, 4) NULL,
	[Vlr_IcmParDes] [numeric](18, 4) NULL,
	[Vlr_IcmParRem] [numeric](18, 4) NULL,
	[Cod_Rota] [int] NULL,
	[Chv_AcessoRel] [varchar](44) NULL,
	[Vlr_DspExt] [numeric](18, 4) NULL,
	[Num_InscriSubTriEmiUfd] [varchar](15) NULL,
	[Cod_CtaPagGnr] [int] NULL,
	[Cod_RamAtv] [int] NULL,
	[Num_SerECF] [varchar](9) NULL,
	[Vlr_DscBonDup] [numeric](18, 4) NULL,
	[Sta_IntWms] [varchar](1) NULL,
	[Flg_CanPdv] [bit] NULL,
	[Vlr_IcmsDif] [numeric](18, 4) NULL,
	[Vlr_DebVrbVdr] [numeric](18, 4) NOT NULL,
	[Inscricao_Suframa] [varchar](9) NULL,
	[Vlr_IcmsDeson] [numeric](18, 4) NOT NULL,
	[Vlr_DscCalSuframa] [numeric](18, 4) NOT NULL,
	[Vlr_FcpIcm] [numeric](18, 4) NULL,
	[Vlr_FcpSbt] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRet] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRec] [numeric](18, 4) NULL,
	[Flg_CalTrbDevCfg] [bit] NULL,
	[Seq_cNF] [bigint] NULL,
	[Vlr_DspDevST] [numeric](18, 4) NULL,
	[Flg_Importado] [bit] NOT NULL,
	[Tip_GerVrb] [varchar](1) NULL,
	[Per_LucLiq] [numeric](18, 4) NULL,
	[Per_LucBru] [numeric](18, 4) NULL,
	[Vlr_VrbOpe] [numeric](18, 4) NULL,
	[Flg_ImportadorNfe] [bit] NULL,
	[Flg_PrcUniAcrIcm] [bit] NULL,
	[Id_Consig] [int] NULL,
	[Num_SeqDev] [smallint] NULL,
	[Vlr_BasDevRecSbt] [numeric](18, 4) NULL,
	[Vlr_DevRecSbt] [numeric](18, 4) NULL,
	[Chv_NotOri] [varchar](44) NULL,
	[Flg_EnvTra] [bit] NULL,
	[Id_Pais] [smallint] NULL,
	[Flg_IncIpiBasCalPis] [bit] NULL,
	[Flg_IncIpiBasCalIcm] [bit] NULL,
	[Flg_BlqCalAutTrb] [bit] NULL,
	[Cod_OriDesNfs] [varchar](1) NULL,
	[Id_OpeVdo] [int] NULL,
	[Vlr_BasIrf] [numeric](18, 4) NULL,
	[Vlr_Irf] [numeric](18, 4) NULL,
	[Flg_BlqTrfEstFis] [bit] NULL,
	[Nom_UsuFat] [varchar](15) NULL,
	[Vlr_BasCsl] [numeric](18, 4) NULL,
	[Vlr_Csl] [numeric](18, 4) NULL,
 CONSTRAINT [PK_R_NFSCB] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Ser_Nota] ASC,
	[Num_Nota] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Estabe]  DEFAULT (0) FOR [Cod_Estabe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_PagNF]  DEFAULT (0) FOR [Qtd_PagNF]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_PrzMed]  DEFAULT (0) FOR [Qtd_PrzMed]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_Parcela]  DEFAULT (0) FOR [Qtd_Parcela]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Cfo1]  DEFAULT (0) FOR [Cod_Cfo1]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_Faturado]  DEFAULT (0) FOR [Per_Faturado]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasPar]  DEFAULT (0) FOR [Vlr_BasPar]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasDscCom]  DEFAULT (0) FOR [Vlr_BasDscCom]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_LiqItens]  DEFAULT (0) FOR [Vlr_LiqItens]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_VlrBruItens]  DEFAULT (0) FOR [VlrBruItens]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_RepIcms]  DEFAULT (0) FOR [Vlr_RepIcms]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_SubsTrib]  DEFAULT (0) FOR [Vlr_SubsTrib]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_DescontoCom]  DEFAULT (0) FOR [Per_DescontoCom]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_DescontoFin]  DEFAULT (0) FOR [Per_DescontoFin]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_DescontoCom]  DEFAULT (0) FOR [Vlr_DescontoCom]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_DscTri]  DEFAULT (0) FOR [Vlr_DscTri]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_TotalNota]  DEFAULT (0) FOR [Vlr_TotalNota]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasIcmsNor]  DEFAULT (0) FOR [Vlr_BasIcmsNor]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_IcmsNor]  DEFAULT (0) FOR [Vlr_IcmsNor]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasIcmsTri]  DEFAULT (0) FOR [Vlr_BasIcmsTri]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_IcmsTri]  DEFAULT (0) FOR [Vlr_IcmsTri]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasRepIcms]  DEFAULT (0) FOR [Vlr_BasRepIcms]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_PrdSubTri]  DEFAULT (0) FOR [Vlr_PrdSubTri]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasSubsTrib]  DEFAULT (0) FOR [Vlr_BasSubsTrib]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Ipi]  DEFAULT (0) FOR [Vlr_Ipi]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Isento]  DEFAULT (0) FOR [Vlr_Isento]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_IcmsSobreTotal]  DEFAULT (0) FOR [Flg_IcmsSobreTotal]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_Peso]  DEFAULT (0) FOR [Qtd_Peso]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_Volumes]  DEFAULT (0) FOR [Qtd_Volumes]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_Itens]  DEFAULT (0) FOR [Qtd_Itens]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_ComisNormal]  DEFAULT (0) FOR [Flg_ComisNormal]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_MovEst]  DEFAULT (0) FOR [Flg_MovEst]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComPag]  DEFAULT (0) FOR [Vlr_ComPag]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComPagTlmkt]  DEFAULT (0) FOR [Vlr_ComPagTlmkt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_ComVnd]  DEFAULT (0) FOR [Per_ComVnd]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComTlmkt]  DEFAULT (0) FOR [Vlr_ComTlmkt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Comissao]  DEFAULT (0) FOR [Vlr_Comissao]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_NotSubFat]  DEFAULT (0) FOR [Num_NotSubFat]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Outros]  DEFAULT (0) FOR [Vlr_Outros]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Seguro]  DEFAULT (0) FOR [Vlr_Seguro]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_OutDsp]  DEFAULT (0) FOR [Vlr_OutDsp]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Frete]  DEFAULT (0) FOR [Vlr_Frete]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasIpi]  DEFAULT (0) FOR [Vlr_BasIpi]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_RetEnt]  DEFAULT (0) FOR [Flg_RetEnt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Val_ComTra]  DEFAULT (0) FOR [Val_ComTra]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Val_ComPagTra]  DEFAULT (0) FOR [Val_ComPagTra]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Digitador]  DEFAULT (0) FOR [Cod_Digitador]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_DscFinLiq]  DEFAULT (0) FOR [Per_DscFinLiq]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_WMS]  DEFAULT (0) FOR [Flg_WMS]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Id_Lote]  DEFAULT (0) FOR [Id_Lote]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_Protoc]  DEFAULT (0) FOR [Num_Protoc]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_StaResp]  DEFAULT (0) FOR [Cod_StaResp]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Ret_CStat]  DEFAULT (0) FOR [Ret_CStat]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Tam_Xml]  DEFAULT (0) FOR [Tam_Xml]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_Email]  DEFAULT (0) FOR [Flg_Email]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_CST_Pis_ST]  DEFAULT (0) FOR [Flg_CST_Pis_ST]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_CST_Cof_ST]  DEFAULT (0) FOR [Flg_CST_Cof_ST]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Pis]  DEFAULT (0) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Cofins]  DEFAULT (0) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_Storage]  DEFAULT (0) FOR [Flg_Storage]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasRecSbt]  DEFAULT (0) FOR [Vlr_BasRecSbt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_RecSbt]  DEFAULT (0) FOR [Vlr_RecSbt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Qtd_PesBru]  DEFAULT (0) FOR [Qtd_PesBru]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_SubsTribEmb]  DEFAULT (0) FOR [Vlr_SubsTribEmb]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Id_PolCom]  DEFAULT (0) FOR [Id_PolCom]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_BlqImpVol]  DEFAULT (0) FOR [Flg_BlqImpVol]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_Verba]  DEFAULT (0) FOR [Vlr_Verba]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasRecSbtInt]  DEFAULT (0) FOR [Vlr_BasRecSbtInt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_RecSbtInt]  DEFAULT (0) FOR [Vlr_RecSbtInt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_Caixa]  DEFAULT (0) FOR [Num_Caixa]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_Turno]  DEFAULT (0) FOR [Num_Turno]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_ForPag]  DEFAULT (0) FOR [Cod_ForPag]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_EntDom]  DEFAULT (0) FOR [Cod_EntDom]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Operad]  DEFAULT (0) FOR [Cod_Operad]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Orcame]  DEFAULT (0) FOR [Cod_Orcame]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_Docume]  DEFAULT (0) FOR [Num_Docume]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_MovDev]  DEFAULT (0) FOR [Cod_MovDev]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Entreg]  DEFAULT (0) FOR [Cod_Entreg]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_IsnNfs]  DEFAULT (0) FOR [Num_IsnNfs]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Val_Subsid]  DEFAULT (0) FOR [Val_Subsid]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Val_Troca]  DEFAULT (0) FOR [Val_Troca]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_Retorn]  DEFAULT (0) FOR [Flg_Retorn]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_Estorn]  DEFAULT (0) FOR [Flg_Estorn]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_Altera]  DEFAULT (0) FOR [Flg_Altera]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_RegTri]  DEFAULT (0) FOR [Cod_RegTri]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT (0) FOR [Flg_RepNfePrc]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_cMsg]  DEFAULT (0) FOR [cMsg]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Contrato]  DEFAULT ((0)) FOR [Cod_Contrato]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_SequenciaCtr]  DEFAULT ((0)) FOR [Num_SequenciaCtr]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_FrePrv]  DEFAULT ((0)) FOR [Vlr_FrePrv]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_ExpBomPrc]  DEFAULT ((0)) FOR [Flg_ExpBomPrc]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_BasSbtRes]  DEFAULT ((0)) FOR [Vlr_BasSbtRes]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_SbtRes]  DEFAULT ((0)) FOR [Vlr_SbtRes]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Gerencia]  DEFAULT ((0)) FOR [Cod_Gerencia]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_ComGer]  DEFAULT ((0)) FOR [Per_ComGer]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComGer]  DEFAULT ((0)) FOR [Vlr_ComGer]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComPagGer]  DEFAULT ((0)) FOR [Vlr_ComPagGer]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Supervisor]  DEFAULT ((0)) FOR [Cod_Supervisor]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_ComSup]  DEFAULT ((0)) FOR [Per_ComSup]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComSup]  DEFAULT ((0)) FOR [Vlr_ComSup]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComPagSup]  DEFAULT ((0)) FOR [Vlr_ComPagSup]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_GerOpe]  DEFAULT ((0)) FOR [Cod_GerOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_ComGerOpe]  DEFAULT ((0)) FOR [Per_ComGerOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComGerOpe]  DEFAULT ((0)) FOR [Vlr_ComGerOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComPagGerOpe]  DEFAULT ((0)) FOR [Vlr_ComPagGerOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_SupOpe]  DEFAULT ((0)) FOR [Cod_SupOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_ComSupOpe]  DEFAULT ((0)) FOR [Per_ComSupOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComSupOpe]  DEFAULT ((0)) FOR [Vlr_ComSupOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ComPagSupOpe]  DEFAULT ((0)) FOR [Vlr_ComPagSupOpe]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_VrbPar]  DEFAULT ((0)) FOR [Vlr_VrbPar]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_VrbBon]  DEFAULT ((0)) FOR [Vlr_VrbBon]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Id_Transacao]  DEFAULT ((0)) FOR [Id_Transacao]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_DscBon]  DEFAULT ((0)) FOR [Vlr_DscBon]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_VrbVdr]  DEFAULT ((0)) FOR [Vlr_VrbVdr]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_VrbSup]  DEFAULT ((0)) FOR [Vlr_VrbSup]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Num_SeqBal]  DEFAULT ((0)) FOR [Num_SeqBal]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ResIcmSbtIntSN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_ResIcmSbtExtEN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_DepMovEst]  DEFAULT ((0)) FOR [Cod_DepMovEst]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_ZonMovEst]  DEFAULT ((0)) FOR [Cod_ZonMovEst]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_LayoutPde]  DEFAULT ((0)) FOR [Cod_LayoutPde]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_IcmFcpDes]  DEFAULT ((0)) FOR [Vlr_IcmFcpDes]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_IcmParDes]  DEFAULT ((0)) FOR [Vlr_IcmParDes]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_IcmParRem]  DEFAULT ((0)) FOR [Vlr_IcmParRem]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_Rota]  DEFAULT ((0)) FOR [Cod_Rota]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_DspExt]  DEFAULT ((0)) FOR [Vlr_DspExt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_CtaPagGnr]  DEFAULT ((0)) FOR [Cod_CtaPagGnr]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Cod_RamAtv]  DEFAULT ((0)) FOR [Cod_RamAtv]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_DscBonDup]  DEFAULT ((0)) FOR [Vlr_DscBonDup]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Flg_CanPdv]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Vlr_IcmsDif]  DEFAULT ((0)) FOR [Vlr_IcmsDif]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_DebVrbVdr]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_IcmsDeson]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_DscCalSuframa]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_FcpIcm]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRec]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_CalTrbDevCfg]  DEFAULT ((0)) FOR [Flg_CalTrbDevCfg]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Seq_cNF]  DEFAULT ((0)) FOR [Seq_cNF]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_DspDevST]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_Importado]  DEFAULT ((0)) FOR [Flg_Importado]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_LucLiq]  DEFAULT ((0)) FOR [Per_LucLiq]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Per_LucBru]  DEFAULT ((0)) FOR [Per_LucBru]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_PrcUniAcrIcm]  DEFAULT ((0)) FOR [Flg_PrcUniAcrIcm]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_BasDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_DevRecSbt]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_EnvTra]  DEFAULT ((0)) FOR [Flg_EnvTra]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Id_Pais]  DEFAULT ((0)) FOR [Id_Pais]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_IncIpiBasCalPis]  DEFAULT ((0)) FOR [Flg_IncIpiBasCalPis]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_IncIpiBasCalIcm]  DEFAULT ((0)) FOR [Flg_IncIpiBasCalIcm]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Flg_BlqCalAutTrb]  DEFAULT ((0)) FOR [Flg_BlqCalAutTrb]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  CONSTRAINT [DF_R_NFSCB_Id_OpeVdo]  DEFAULT ((0)) FOR [Id_OpeVdo]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_BasIrf]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_Irf]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Flg_BlqTrfEstFis]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_BasCsl]
GO

ALTER TABLE [dbo].[R_NFSCB] ADD  DEFAULT ((0)) FOR [Vlr_Csl]
GO


