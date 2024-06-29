USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_NFECB]    Script Date: 28/06/2024 08:49:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_NFECB](
	[Protocolo] [int] NOT NULL,
	[Status] [char](1) NULL,
	[Tip_Emitente] [char](1) NULL,
	[Cod_EmiFornec] [int] NULL,
	[Cod_EmiTransp] [int] NULL,
	[Cod_EmiCliente] [int] NULL,
	[Pessoa] [char](1) NULL,
	[Cgc_Cpf] [varchar](14) NULL,
	[Cgf_Rg] [varchar](15) NULL,
	[Endereco] [varchar](50) NULL,
	[Bairro] [varchar](20) NULL,
	[Cidade] [varchar](25) NULL,
	[Cod_UfOrigem] [varchar](2) NULL,
	[Cep] [varchar](8) NULL,
	[Cod_RegTri] [int] NULL,
	[Especie] [varchar](3) NULL,
	[Serie] [varchar](3) NULL,
	[Numero] [int] NULL,
	[Dat_Emissao] [smalldatetime] NULL,
	[Dat_Entrada] [smalldatetime] NULL,
	[Dat_Movimento] [smalldatetime] NULL,
	[Cod_Cfo] [int] NULL,
	[Tip_NF] [char](1) NULL,
	[Tip_RecST] [char](1) NULL,
	[Cod_Pedido] [int] NULL,
	[Vlr_Despesas] [numeric](18, 4) NULL,
	[Vlr_Mercadoria] [numeric](18, 4) NULL,
	[Vlr_Desconto] [numeric](18, 4) NULL,
	[Vlr_Nota] [numeric](18, 4) NULL,
	[Cod_VlrFiscal] [char](1) NULL,
	[Vlr_BasIcmsTri] [numeric](18, 4) NULL,
	[Vlr_IcmsTri] [numeric](18, 4) NULL,
	[Vlr_BasIcmsNor] [numeric](18, 4) NULL,
	[Vlr_IcmsNor] [numeric](18, 4) NULL,
	[Vlr_PrdSubTri] [numeric](18, 4) NULL,
	[Vlr_BasIstFon] [numeric](18, 4) NULL,
	[Vlr_IstFon] [numeric](18, 4) NULL,
	[Vlr_PrdAntRec] [numeric](18, 4) NULL,
	[Vlr_BasAntRec] [numeric](18, 4) NULL,
	[Vlr_AntRec] [numeric](18, 4) NULL,
	[Vlr_BasIpi] [numeric](18, 4) NULL,
	[Vlr_Ipi] [numeric](18, 4) NULL,
	[Vlr_Isento] [numeric](18, 4) NULL,
	[Vlr_BasDifTri] [numeric](18, 4) NULL,
	[Vlr_DifTri] [numeric](18, 4) NULL,
	[Cod_Transp] [int] NULL,
	[Ser_Frete] [varchar](3) NULL,
	[Num_Frete] [int] NULL,
	[Dat_EmiFrete] [smalldatetime] NULL,
	[Dat_VctFrete] [smalldatetime] NULL,
	[Vlr_BasIcmFrete] [numeric](18, 4) NULL,
	[Alq_IcmFrete] [numeric](18, 8) NULL,
	[Vlr_IcmFrete] [numeric](18, 4) NULL,
	[Vlr_Frete] [numeric](18, 4) NULL,
	[Ser_SeloFisTran] [varchar](2) NULL,
	[Num_SeloFisTran] [bigint] NULL,
	[Dat_SeloFisTran] [smalldatetime] NULL,
	[Observacao] [varchar](80) NULL,
	[Flg_FecCab] [bit] NULL,
	[Flg_FecIte] [bit] NULL,
	[Num_Formulario] [int] NULL,
	[Dat_Cancelamento] [smalldatetime] NULL,
	[Qtd_PagNf] [int] NULL,
	[Qtd_Itens] [int] NULL,
	[Flg_ImpNot] [bit] NULL,
	[Ser_NfsOri] [varchar](3) NULL,
	[Num_NfsOri] [int] NULL,
	[Flg_DevTot] [bit] NULL,
	[Tip_Consumidor] [varchar](1) NULL,
	[Cod_Vendedor] [int] NULL,
	[Cod_VendTlmkt] [int] NULL,
	[Per_Comvnd] [numeric](18, 8) NULL,
	[Flg_ComisNormal] [bit] NULL,
	[Vlr_Comissao] [numeric](18, 4) NULL,
	[Vlr_ComTlmkt] [numeric](18, 4) NULL,
	[Flg_Movest] [bit] NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[Str_RelDoc] [varchar](42) NULL,
	[Vlr_BasRepIcms] [numeric](18, 4) NULL,
	[Vlr_RepIcms] [numeric](18, 4) NULL,
	[Vlr_Outros] [numeric](18, 4) NULL,
	[Vlr_Seguro] [numeric](18, 4) NULL,
	[Vlr_OutDsp] [numeric](18, 4) NULL,
	[Cod_Ope] [varchar](3) NULL,
	[Tip_DocRel] [varchar](1) NULL,
	[Tip_Frete] [varchar](1) NULL,
	[Via_Transp] [varchar](1) NULL,
	[Cod_PreEnt] [int] NULL,
	[Cod_ValCre] [int] NULL,
	[Obs_Rodape] [text] NULL,
	[Obs_Corpo] [text] NULL,
	[Flg_WMS] [bit] NULL,
	[Vlr_DspExtNotFis] [numeric](18, 4) NULL,
	[Vlr_DscExtNotFis] [numeric](18, 4) NULL,
	[Flg_AtuPrcCus] [bit] NULL,
	[Cod_CidIbge] [varchar](7) NULL,
	[Cod_ModEmiNfe] [char](1) NULL,
	[Cod_ModImpDanfe] [char](2) NULL,
	[Cod_FinEmi] [char](1) NULL,
	[Cod_ModDoc] [char](2) NULL,
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
	[Flg_Email] [bit] NULL,
	[Flg_CST_Pis_ST] [bit] NULL,
	[Flg_CST_Cof_ST] [bit] NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[Flg_Storage] [bit] NULL,
	[Qtd_PesBru] [numeric](18, 4) NULL,
	[Qtd_Peso] [numeric](18, 4) NULL,
	[Qtd_Volumes] [int] NULL,
	[Des_JusCan] [varchar](255) NULL,
	[Cod_FinNfe] [char](1) NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Num_Caixa] [smallint] NULL,
	[Num_Turno] [smallint] NULL,
	[Cod_Operad] [int] NULL,
	[Tip_DevEcf] [varchar](1) NULL,
	[Flg_RepNfePrc] [bit] NOT NULL,
	[Vlr_DspSbt] [numeric](18, 4) NULL,
	[cMsg] [int] NULL,
	[xMsg] [varchar](1200) NULL,
	[Vlr_BasSbtRes] [numeric](18, 4) NULL,
	[Vlr_SbtRes] [numeric](18, 4) NULL,
	[Vlr_BasSuframa] [numeric](18, 4) NULL,
	[Vlr_Suframa] [numeric](18, 4) NULL,
	[Vlr_BasDevRecSbt] [numeric](18, 4) NULL,
	[Vlr_DevRecSbt] [numeric](18, 4) NULL,
	[Flg_Estorno] [bit] NULL,
	[Des_InfAdFisco] [varchar](2000) NULL,
	[Id_Transacao] [int] NULL,
	[Cod_MtvDev] [varchar](5) NULL,
	[Des_MtvDev] [varchar](40) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Num_SeqBal] [int] NULL,
	[Vlr_ResIcmSbtIntSN] [numeric](18, 4) NULL,
	[Vlr_ResIcmSbtExtEN] [numeric](18, 4) NULL,
	[Xml_Cancelamento] [ntext] NULL,
	[Chv_AcessoRel] [varchar](44) NULL,
	[Flg_CnfFis] [bit] NULL,
	[Flg_CnfCom] [bit] NULL,
	[Sta_CnfEst] [char](1) NULL,
	[Cod_Confer] [int] NULL,
	[Hor_IniCnf] [smalldatetime] NULL,
	[Hor_FimCnf] [smalldatetime] NULL,
	[Vlr_IcmFcpDes] [numeric](18, 4) NULL,
	[Vlr_IcmParDes] [numeric](18, 4) NULL,
	[Vlr_IcmParRem] [numeric](18, 4) NULL,
	[Vlr_BasDspExt] [numeric](18, 4) NULL,
	[Vlr_DspExt] [numeric](18, 4) NULL,
	[Status_2] [varchar](1) NULL,
	[Flg_Importado] [bit] NULL,
	[Id_PdvCon] [int] NOT NULL,
	[Cod_FinCte] [char](1) NULL,
	[Sta_IntWms] [varchar](1) NULL,
	[Cod_NatCtr] [varchar](1) NULL,
	[Vlr_Verba] [numeric](18, 4) NULL,
	[Vlr_IcmsDif] [numeric](18, 4) NULL,
	[Inscricao_Suframa] [varchar](9) NULL,
	[Vlr_IcmsDeson] [numeric](18, 4) NOT NULL,
	[Vlr_DscCalSuframa] [numeric](18, 4) NOT NULL,
	[Vlr_FcpIcm] [numeric](18, 4) NULL,
	[Vlr_FcpSbt] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRet] [numeric](18, 4) NULL,
	[Vlr_FcpSbtRec] [numeric](18, 4) NULL,
	[Vlr_FcpSbtAnt] [numeric](18, 4) NULL,
	[Flg_CalTrbDevCfg] [bit] NULL,
	[Seq_cNF] [bigint] NULL,
	[Vlr_DspDevST] [numeric](18, 4) NULL,
	[Flg_DscIcmDesone] [bit] NULL,
	[Vlr_DscTri] [numeric](18, 4) NULL,
	[Flg_CrossDocking] [bit] NULL,
	[Qtd_PrzMed] [numeric](18, 4) NULL,
	[Cod_CtaPagGnr] [int] NULL,
	[Flg_ImportadorNfe] [bit] NULL,
	[Vlr_FreCte] [numeric](18, 4) NULL,
	[Id_Consig] [int] NULL,
	[Num_SeqCtr] [int] NULL,
	[Num_SeqDev] [smallint] NULL,
	[Chv_NotOri] [varchar](44) NULL,
	[Id_Pais] [smallint] NULL,
	[_Num_IdeTin] [varchar](20) NULL,
	[Id_DclImp] [bigint] NULL,
	[_Dat_EmiDuimp] [smalldatetime] NULL,
	[_Des_LocDesembAduane] [varchar](50) NULL,
	[_Cod_EstDesembAduane] [varchar](2) NULL,
	[_Dat_DesembAduane] [smalldatetime] NULL,
	[Flg_IncIpiBasCalPis] [bit] NULL,
	[Flg_IncIpiBasCalIcm] [bit] NULL,
	[Flg_BlqCalAutTrb] [bit] NULL,
	[Cod_OriDesNfs] [varchar](1) NULL,
	[Id_OpeVdo] [int] NULL,
	[Id_EndDep] [int] NULL,
	[Dat_PreRec] [smalldatetime] NULL,
	[Vlr_ComSup] [numeric](18, 4) NULL,
	[Vlr_ComSupOpe] [numeric](18, 4) NULL,
	[Vlr_ComGer] [numeric](18, 4) NULL,
	[Vlr_ComGerOpe] [numeric](18, 4) NULL,
	[Vlr_VrbVdr] [numeric](18, 4) NULL,
	[Vlr_VrbOpe] [numeric](18, 4) NULL,
	[Vlr_VrbSup] [numeric](18, 4) NULL,
	[Cod_Supervisor] [int] NULL,
	[Cod_SupOpe] [int] NULL,
	[Cod_Gerencia] [int] NULL,
	[Cod_GerOpe] [int] NULL,
	[Flg_BlqTrfEstFis] [bit] NULL,
	[Flg_DscBonDup] [bit] NULL,
	[Vlr_DscBon] [numeric](18, 4) NULL,
 CONSTRAINT [PK_R_NFECB] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Protocolo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Despesas]  DEFAULT (0) FOR [Vlr_Despesas]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Mercadoria]  DEFAULT (0) FOR [Vlr_Mercadoria]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Desconto]  DEFAULT (0) FOR [Vlr_Desconto]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Nota]  DEFAULT (0) FOR [Vlr_Nota]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasIcmsTri]  DEFAULT (0) FOR [Vlr_BasIcmsTri]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmsTri]  DEFAULT (0) FOR [Vlr_IcmsTri]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasIcmsNor]  DEFAULT (0) FOR [Vlr_BasIcmsNor]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmsNor]  DEFAULT (0) FOR [Vlr_IcmsNor]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_PrdSubTri]  DEFAULT (0) FOR [Vlr_PrdSubTri]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasIstFon]  DEFAULT (0) FOR [Vlr_BasIstFon]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IstFon]  DEFAULT (0) FOR [Vlr_IstFon]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_PrdAntRec]  DEFAULT (0) FOR [Vlr_PrdAntRec]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasAntRec]  DEFAULT (0) FOR [Vlr_BasAntRec]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_AntRec]  DEFAULT (0) FOR [Vlr_AntRec]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasIpi]  DEFAULT (0) FOR [Vlr_BasIpi]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Ipi]  DEFAULT (0) FOR [Vlr_Ipi]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Isento]  DEFAULT (0) FOR [Vlr_Isento]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasDifTri]  DEFAULT (0) FOR [Vlr_BasDifTri]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DifTri]  DEFAULT (0) FOR [Vlr_DifTri]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasIcmFrete]  DEFAULT (0) FOR [Vlr_BasIcmFrete]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Alq_IcmFrete]  DEFAULT (0) FOR [Alq_IcmFrete]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmFrete]  DEFAULT (0) FOR [Vlr_IcmFrete]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Frete]  DEFAULT (0) FOR [Vlr_Frete]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_FecCab]  DEFAULT (0) FOR [Flg_FecCab]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_FecIte]  DEFAULT (0) FOR [Flg_FecIte]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Qtd_PagNf]  DEFAULT (0) FOR [Qtd_PagNf]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Qtd_Itens]  DEFAULT (0) FOR [Qtd_Itens]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_ImpNot]  DEFAULT (0) FOR [Flg_ImpNot]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_DevTot]  DEFAULT (0) FOR [Flg_DevTot]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_ComisNormal]  DEFAULT (0) FOR [Flg_ComisNormal]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Comissao]  DEFAULT (0) FOR [Vlr_Comissao]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_ComTlmkt]  DEFAULT (0) FOR [Vlr_ComTlmkt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_Movest]  DEFAULT (0) FOR [Flg_Movest]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasRepIcms]  DEFAULT (0) FOR [Vlr_BasRepIcms]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_RepIcms]  DEFAULT (0) FOR [Vlr_RepIcms]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Outros]  DEFAULT (0) FOR [Vlr_Outros]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Seguro]  DEFAULT (0) FOR [Vlr_Seguro]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_OutDsp]  DEFAULT (0) FOR [Vlr_OutDsp]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Cod_ValCre]  DEFAULT (0) FOR [Cod_ValCre]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_WMS]  DEFAULT (0) FOR [Flg_WMS]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DspExtNotFis]  DEFAULT (0) FOR [Vlr_DspExtNotFis]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DscExtNotFis]  DEFAULT (0) FOR [Vlr_DscExtNotFis]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_AtuPrcCus]  DEFAULT (0) FOR [Flg_AtuPrcCus]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Id_Lote]  DEFAULT (0) FOR [Id_Lote]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Num_Protoc]  DEFAULT (0) FOR [Num_Protoc]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Cod_StaResp]  DEFAULT (0) FOR [Cod_StaResp]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Ret_CStat]  DEFAULT (0) FOR [Ret_CStat]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Tam_Xml]  DEFAULT (0) FOR [Tam_Xml]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_Email]  DEFAULT (0) FOR [Flg_Email]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_CST_Pis_ST]  DEFAULT (0) FOR [Flg_CST_Pis_ST]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_CST_Cof_ST]  DEFAULT (0) FOR [Flg_CST_Cof_ST]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Pis]  DEFAULT (0) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Cofins]  DEFAULT (0) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_Storage]  DEFAULT (0) FOR [Flg_Storage]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Qtd_PesBru]  DEFAULT (0) FOR [Qtd_PesBru]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Qtd_Peso]  DEFAULT (0) FOR [Qtd_Peso]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Qtd_Volumes]  DEFAULT (0) FOR [Qtd_Volumes]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Cod_Estabe]  DEFAULT (0) FOR [Cod_Estabe]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Num_Caixa]  DEFAULT (0) FOR [Num_Caixa]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Num_Turno]  DEFAULT (0) FOR [Num_Turno]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Cod_Operad]  DEFAULT (0) FOR [Cod_Operad]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT (0) FOR [Flg_RepNfePrc]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DspSbt]  DEFAULT (0) FOR [Vlr_DspSbt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_cMsg]  DEFAULT (0) FOR [cMsg]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasSbtRes]  DEFAULT ((0)) FOR [Vlr_BasSbtRes]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_SbtRes]  DEFAULT ((0)) FOR [Vlr_SbtRes]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasSuframa]  DEFAULT ((0)) FOR [Vlr_BasSuframa]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Suframa]  DEFAULT ((0)) FOR [Vlr_Suframa]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasDevRecSbt]  DEFAULT ((0)) FOR [Vlr_BasDevRecSbt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DevRecSbt]  DEFAULT ((0)) FOR [Vlr_DevRecSbt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_Estorno]  DEFAULT ((0)) FOR [Flg_Estorno]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Id_Transacao]  DEFAULT ((0)) FOR [Id_Transacao]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Num_SeqBal]  DEFAULT ((0)) FOR [Num_SeqBal]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_ResIcmSbtIntSN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtIntSN]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_ResIcmSbtExtEN]  DEFAULT ((0)) FOR [Vlr_ResIcmSbtExtEN]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_CnfFis]  DEFAULT ((0)) FOR [Flg_CnfFis]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_CnfCom]  DEFAULT ((0)) FOR [Flg_CnfCom]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmFcpDes]  DEFAULT ((0)) FOR [Vlr_IcmFcpDes]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmParDes]  DEFAULT ((0)) FOR [Vlr_IcmParDes]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmParRem]  DEFAULT ((0)) FOR [Vlr_IcmParRem]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_BasDspExt]  DEFAULT ((0)) FOR [Vlr_BasDspExt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DspExt]  DEFAULT ((0)) FOR [Vlr_DspExt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_Importado]  DEFAULT ((0)) FOR [Flg_Importado]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Id_PdvCon]  DEFAULT ((0)) FOR [Id_PdvCon]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_Verba]  DEFAULT ((0)) FOR [Vlr_Verba]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_IcmsDif]  DEFAULT ((0)) FOR [Vlr_IcmsDif]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_IcmsDeson]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_DscCalSuframa]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRet]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtRec]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_FcpSbtAnt]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_CalTrbDevCfg]  DEFAULT ((0)) FOR [Flg_CalTrbDevCfg]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Seq_cNF]  DEFAULT ((0)) FOR [Seq_cNF]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_DspDevST]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_DscIcmDesone]  DEFAULT ((0)) FOR [Flg_DscIcmDesone]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Vlr_DscTri]  DEFAULT ((0)) FOR [Vlr_DscTri]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_CrossDocking]  DEFAULT ((0)) FOR [Flg_CrossDocking]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Cod_CtaPagGnr]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Id_Pais]  DEFAULT ((0)) FOR [Id_Pais]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Id_Duimp]  DEFAULT ((0)) FOR [Id_DclImp]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_IncIpiBasCalPis]  DEFAULT ((0)) FOR [Flg_IncIpiBasCalPis]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_IncIpiBasCalIcm]  DEFAULT ((0)) FOR [Flg_IncIpiBasCalIcm]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Flg_BlqCalAutTrb]  DEFAULT ((0)) FOR [Flg_BlqCalAutTrb]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  CONSTRAINT [DF_R_NFECB_Id_OpeVdo]  DEFAULT ((0)) FOR [Id_OpeVdo]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Id_EndDep]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_ComSup]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_ComSupOpe]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_ComGer]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_ComGerOpe]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_VrbVdr]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_VrbOpe]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_VrbSup]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Cod_Supervisor]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Cod_SupOpe]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Cod_Gerencia]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Cod_GerOpe]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Flg_BlqTrfEstFis]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Flg_DscBonDup]
GO

ALTER TABLE [dbo].[R_NFECB] ADD  DEFAULT ((0)) FOR [Vlr_DscBon]
GO


