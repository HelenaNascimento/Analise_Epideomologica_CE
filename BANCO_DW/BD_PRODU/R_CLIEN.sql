USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_CLIEN]    Script Date: 28/06/2024 08:56:37 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_R_CLIEN](
	[Codigo] [int] NOT NULL,
	[Razao_Social] [varchar](80) NULL,
	[Pessoa] [char](1) NULL,
	[Cgc_Cpf] [varchar](14) NULL,
	[Cgc_Matriz] [varchar](8) NULL,
	[Cgf] [varchar](15) NULL,
	[Fantasia] [varchar](80) NULL,
	[Endereco] [varchar](35) NULL,
	[Cod_Estado] [char](2) NULL,
	[Cod_Cidade] [int] NULL,
	[Cod_Bairro] [int] NULL,
	[Cep] [char](8) NULL,
	[_Cod_RegTri] [int] NULL,
	[Fone1] [varchar](20) NULL,
	[Fone2] [varchar](20) NULL,
	[Fax] [varchar](20) NULL,
	[Email] [varchar](120) NULL,
	[Contato] [varchar](20) NULL,
	[Licenca_Saude] [varchar](30) NULL,
	[Val_LicSau] [smalldatetime] NULL,
	[Tipo_Consumidor] [char](1) NULL,
	[Flag_EndCadCob] [bit] NULL,
	[Endereco_Cob] [varchar](35) NULL,
	[Bairro_Cob] [varchar](20) NULL,
	[Cidade_Cob] [varchar](20) NULL,
	[Estado_Cob] [char](2) NULL,
	[Cep_Cob] [char](8) NULL,
	[Sta_ClaAbcVal] [char](1) NULL,
	[Per_ParticFat] [numeric](18, 8) NULL,
	[Data_Cadastro] [smalldatetime] NULL,
	[Limite_Credito] [numeric](18, 4) NULL,
	[Dat_LimCreAtu] [smalldatetime] NULL,
	[Vlr_LimCreAnt] [numeric](18, 4) NULL,
	[Total_Debito] [numeric](18, 4) NULL,
	[Str_PrzVen] [varchar](24) NULL,
	[Per_DscVen] [numeric](18, 8) NULL,
	[Per_DscFinVen] [numeric](18, 8) NULL,
	[Per_DscComVen] [numeric](18, 8) NULL,
	[Flag_ClienteEsp] [bit] NULL,
	[Qtd_PrzMax] [int] NULL,
	[Qtd_ParAtv] [int] NULL,
	[Qtd_PrzMaxDsc] [int] NULL,
	[Per_DscFinMax] [numeric](18, 8) NULL,
	[Per_DscComMaxVis] [numeric](18, 8) NULL,
	[Per_ComVnd] [numeric](18, 8) NULL,
	[Vlr_ObjetivoMes] [numeric](18, 4) NULL,
	[Atraso_Permitido] [int] NULL,
	[Atraso_Atual] [int] NULL,
	[Atraso_MedAtu] [int] NULL,
	[Data_UltimaFatura] [smalldatetime] NULL,
	[Valor_UltimaFatura] [numeric](18, 4) NULL,
	[Valor_MaiorFatura] [numeric](18, 4) NULL,
	[Maior_Atraso] [int] NULL,
	[Cod_RamoAtividade] [int] NULL,
	[Cod_InfCredito] [int] NULL,
	[_Cod_Transportadora] [int] NULL,
	[_Cod_Rota] [int] NULL,
	[_Cod_Agente] [int] NULL,
	[_Cod_Vendedor] [int] NULL,
	[_Cod_OperTlmk] [int] NULL,
	[Bloqueado] [bit] NULL,
	[Motivo_Bloqueio] [varchar](30) NULL,
	[Usuario_Bloqueio] [varchar](15) NULL,
	[Msg_NotaFiscal] [varchar](40) NULL,
	[Observacao] [text] NULL,
	[Cod_LocCtb] [varchar](3) NULL,
	[Cod_PlcCtb] [varchar](15) NULL,
	[Cod_LocCtbDes] [varchar](3) NULL,
	[Cod_PlcCtbDes] [varchar](15) NULL,
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
	[Per_IstOrc] [numeric](18, 8) NULL,
	[Flg_DupExtIst] [bit] NULL,
	[Cod_GrpCli] [int] NULL,
	[Tip_DscPdv] [char](1) NULL,
	[Cod_Ean] [varchar](13) NULL,
	[Flg_ComPrpDsc] [bit] NULL,
	[Flg_ComFab] [bit] NULL,
	[Per_SubFat] [numeric](18, 8) NULL,
	[Sta_ChqVnc] [varchar](1) NULL,
	[Transacao] [smalldatetime] NULL,
	[Usuario] [varchar](35) NULL,
	[Flg_CadPen] [bit] NULL,
	[Dat_UltVis] [smalldatetime] NULL,
	[Qtd_DiaPerVis] [int] NULL,
	[Num_Anvisa] [varchar](24) NULL,
	[Val_Anvisa] [smalldatetime] NULL,
	[Num_CerReg] [varchar](16) NULL,
	[Val_CerReg] [smalldatetime] NULL,
	[Num_AlvFun] [varchar](20) NULL,
	[Val_AlvFun] [smalldatetime] NULL,
	[Flg_RetArqVen] [bit] NULL,
	[Flg_EndCadEnt] [bit] NULL,
	[Endereco_Ent] [varchar](35) NULL,
	[Bairro_Ent] [varchar](20) NULL,
	[Cidade_Ent] [varchar](20) NULL,
	[Estado_Ent] [varchar](2) NULL,
	[Cep_Ent] [varchar](8) NULL,
	[Fone_Ent] [varchar](20) NULL,
	[Fax_Ent] [varchar](20) NULL,
	[Fone_Cob] [varchar](20) NULL,
	[Fax_Cob] [varchar](20) NULL,
	[Cod_CliPag] [int] NULL,
	[Cod_PerVis] [char](1) NULL,
	[Dia_SemMesVis] [smallint] NULL,
	[Hor_Vis] [smallint] NULL,
	[Min_Vis] [smallint] NULL,
	[Cod_TabPrz] [int] NULL,
	[Cod_TabPrc] [int] NULL,
	[Msg_NotFis] [varchar](2000) NULL,
	[Isn_CtaFin] [int] NULL,
	[Flg_BlqVenOrc] [bit] NULL,
	[Qtd_MesMinPrzVctLot] [int] NULL,
	[Flg_BlqDscPrmExtVenVis] [bit] NULL,
	[Flg_BlqPrm] [bit] NULL,
	[Cod_RotVis] [int] NULL,
	[Numero] [varchar](5) NULL,
	[Complemento] [varchar](30) NULL,
	[Numero_Cob] [varchar](5) NULL,
	[Complemento_Cob] [varchar](30) NULL,
	[Numero_Ent] [varchar](5) NULL,
	[Complemento_Ent] [varchar](30) NULL,
	[Inscricao_Municipal] [varchar](15) NULL,
	[Inscricao_SUFRAMA] [varchar](9) NULL,
	[Num_RegCrm] [varchar](14) NULL,
	[Contato_Cob] [varchar](20) NULL,
	[Email_Cob] [varchar](120) NULL,
	[Contato_Ent] [varchar](20) NULL,
	[Email_Ent] [varchar](120) NULL,
	[Flg_BlqProtes] [bit] NULL,
	[Flg_BlqExpFin] [bit] NULL,
	[Flg_BlqCobTxaBan] [bit] NULL,
	[Per_ComOpe] [numeric](18, 4) NULL,
	[Flg_EDM] [bit] NULL,
	[Cod_LayoutBbs] [int] NULL,
	[Ctr_Vencim] [char](1) NULL,
	[Qtd_DiaVct] [smallint] NULL,
	[Dia_Venci1] [smallint] NULL,
	[Dia_Venci2] [smallint] NULL,
	[Dia_Nascim] [smallint] NULL,
	[Mes_Nascim] [smallint] NULL,
	[Ano_Nascim] [smallint] NULL,
	[Sex_Client] [varchar](1) NULL,
	[Flg_NaoJur] [bit] NULL,
	[Flg_CarMag] [bit] NULL,
	[Tip_CliAss] [varchar](1) NULL,
	[Id_PolCom] [int] NULL,
	[Cod_DDD_1] [varchar](2) NULL,
	[Cod_DDD_2] [varchar](2) NULL,
	[Cod_DDD_Ent] [varchar](2) NULL,
	[Cod_DDD_Cob] [varchar](2) NULL,
	[Flg_BlqEmbDscItePrcUni] [bit] NULL,
	[senha_hash] [varchar](32) NULL,
	[Flg_NaoIncSbtPrc] [bit] NULL,
	[Flg_BlqFecVlrPdvNfsSbtEmb] [bit] NULL,
	[Flg_BlqRepIcm] [bit] NULL,
	[Vlr_CapSoc] [numeric](18, 4) NULL,
	[Dia_VctTitEsp] [tinyint] NULL,
	[Dia_SemVctTitEsp] [tinyint] NULL,
	[Flg_ImpSbtEmbPrc] [bit] NULL,
	[Cod_Husi] [varchar](3) NULL,
	[Flg_InfXmlSbtEmbPrc] [bit] NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Des_Token] [varchar](36) NULL,
	[Cod_TipLucro] [varchar](2) NULL,
	[Cod_GrpBbs] [int] NULL,
	[Flg_BlqDupExtSbt] [bit] NULL,
	[Versao] [bigint] NOT NULL,
	[Cod_EstabeOpe] [int] NULL,
	[Flg_BlqLotVarPdv] [bit] NULL,
	[Cod_HisDscDev] [varchar](3) NULL,
	[Cod_LocDevDscDev] [varchar](3) NULL,
	[Cod_CtaDevDscDev] [varchar](15) NULL,
	[Cod_LocCreDscDev] [varchar](3) NULL,
	[Cod_CtaCreDscDev] [varchar](15) NULL,
	[Flg_BlqIncImpPriDup] [bit] NULL,
	[Flg_GerPdvLotVar] [bit] NULL,
	[Dat_Abertura] [smalldatetime] NULL,
	[Email_Comprador] [varchar](120) NULL,
	[Val_InsSuf] [datetime] NULL,
	[Flg_NaoBlqFinPdv] [bit] NOT NULL,
	[Flg_NaoBlqDocPdv] [bit] NOT NULL,
	[Cod_Crt] [char](1) NULL,
	[Cod_CNAE] [varchar](10) NULL,
	[Hor_FuncioAbe] [time](7) NULL,
	[Hor_FuncioFec] [time](7) NULL,
	[Flg_DscIcmDesoneNotFis] [bit] NULL,
	[Qtd_LimChq] [int] NULL,
	[Val_LimChq] [numeric](18, 4) NULL,
	[Id_Pais] [smallint] NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Cod_ClaCli] [int] NULL,
 CONSTRAINT [PK_R_CLIEN] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flag_EndCadCob]  DEFAULT (0) FOR [Flag_EndCadCob]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_ParticFat]  DEFAULT (0) FOR [Per_ParticFat]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Limite_Credito]  DEFAULT (0) FOR [Limite_Credito]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Vlr_LimCreAnt]  DEFAULT (0) FOR [Vlr_LimCreAnt]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Total_Debito]  DEFAULT (0) FOR [Total_Debito]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_DscVen]  DEFAULT (0) FOR [Per_DscVen]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_DscFinVen]  DEFAULT (0) FOR [Per_DscFinVen]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_DscComVen]  DEFAULT (0) FOR [Per_DscComVen]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flag_ClienteEsp]  DEFAULT (0) FOR [Flag_ClienteEsp]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Qtd_PrzMax]  DEFAULT (0) FOR [Qtd_PrzMax]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Qtd_ParAtv]  DEFAULT (0) FOR [Qtd_ParAtv]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Qtd_PrzMaxDsc]  DEFAULT (0) FOR [Qtd_PrzMaxDsc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_DscFinMax]  DEFAULT (0) FOR [Per_DscFinMax]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_DscComMaxVis]  DEFAULT (0) FOR [Per_DscComMaxVis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_ComVnd]  DEFAULT (0) FOR [Per_ComVnd]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Vlr_ObjetivoMes]  DEFAULT (0) FOR [Vlr_ObjetivoMes]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Atraso_Permitido]  DEFAULT (0) FOR [Atraso_Permitido]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Atraso_Atual]  DEFAULT (0) FOR [Atraso_Atual]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Atraso_MedAtu]  DEFAULT (0) FOR [Atraso_MedAtu]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Valor_UltimaFatura]  DEFAULT (0) FOR [Valor_UltimaFatura]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Valor_MaiorFatura]  DEFAULT (0) FOR [Valor_MaiorFatura]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Maior_Atraso]  DEFAULT (0) FOR [Maior_Atraso]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Bloqueado]  DEFAULT (0) FOR [Bloqueado]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_IstOrc]  DEFAULT (0) FOR [Per_IstOrc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_DupExtIst]  DEFAULT (0) FOR [Flg_DupExtIst]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_ComPrpDsc]  DEFAULT (0) FOR [Flg_ComPrpDsc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_ComFab]  DEFAULT (0) FOR [Flg_ComFab]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_SubFat]  DEFAULT (0) FOR [Per_SubFat]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_CadPen]  DEFAULT (0) FOR [Flg_CadPen]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_RetArqVen]  DEFAULT (0) FOR [Flg_RetArqVen]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_EndCadEnt]  DEFAULT (0) FOR [Flg_EndCadEnt]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Dia_SemMesVis]  DEFAULT (0) FOR [Dia_SemMesVis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Hor_Vis]  DEFAULT (0) FOR [Hor_Vis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Min_Vis]  DEFAULT (0) FOR [Min_Vis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Cod_TabPrz]  DEFAULT (0) FOR [Cod_TabPrz]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Cod_TabPrc]  DEFAULT (0) FOR [Cod_TabPrc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Isn_CtaFin]  DEFAULT (0) FOR [Isn_CtaFin]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqVenOrc]  DEFAULT (0) FOR [Flg_BlqVenOrc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Qtd_MesMinPrzVctLot]  DEFAULT (0) FOR [Qtd_MesMinPrzVctLot]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqDscPrmExtVenVis]  DEFAULT (0) FOR [Flg_BlqDscPrmExtVenVis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqPrm]  DEFAULT (0) FOR [Flg_BlqPrm]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Cod_RotVis]  DEFAULT (0) FOR [Cod_RotVis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqProtes]  DEFAULT (0) FOR [Flg_BlqProtes]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqExpFin]  DEFAULT (0) FOR [Flg_BlqExpFin]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqCobTxaBan]  DEFAULT (0) FOR [Flg_BlqCobTxaBan]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Per_ComOpe]  DEFAULT (0) FOR [Per_ComOpe]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_EDM]  DEFAULT (0) FOR [Flg_EDM]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Cod_LayoutBbs]  DEFAULT (0) FOR [Cod_LayoutBbs]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Qtd_DiaVct]  DEFAULT (0) FOR [Qtd_DiaVct]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Dia_Venci1]  DEFAULT (0) FOR [Dia_Venci1]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Dia_Venci2]  DEFAULT (0) FOR [Dia_Venci2]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Dia_Nascim]  DEFAULT (0) FOR [Dia_Nascim]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Mes_Nascim]  DEFAULT (0) FOR [Mes_Nascim]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Ano_Nascim]  DEFAULT (0) FOR [Ano_Nascim]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_NaoJur]  DEFAULT (0) FOR [Flg_NaoJur]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_CarMag]  DEFAULT (0) FOR [Flg_CarMag]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Id_PolCom]  DEFAULT (0) FOR [Id_PolCom]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqEmbDscItePrcUni]  DEFAULT (0) FOR [Flg_BlqEmbDscItePrcUni]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_NaoIncSbtPrc]  DEFAULT ((0)) FOR [Flg_NaoIncSbtPrc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqFecVlrPdvNfsSbtEmb]  DEFAULT ((0)) FOR [Flg_BlqFecVlrPdvNfsSbtEmb]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqRepIcm]  DEFAULT ((0)) FOR [Flg_BlqRepIcm]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Vlr_CapSoc]  DEFAULT ((0)) FOR [Vlr_CapSoc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Dia_VctTitEsp]  DEFAULT ((0)) FOR [Dia_VctTitEsp]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Dia_SemVctTitEsp]  DEFAULT ((0)) FOR [Dia_SemVctTitEsp]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_ImpSbtEmbPrc]  DEFAULT ((0)) FOR [Flg_ImpSbtEmbPrc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_InfXmlSbtEmbPrc]  DEFAULT ((0)) FOR [Flg_InfXmlSbtEmbPrc]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Cod_GrpBbs]  DEFAULT ((0)) FOR [Cod_GrpBbs]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Versao]  DEFAULT ((0)) FOR [Versao]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Cod_EstabeOpe]  DEFAULT ((0)) FOR [Cod_EstabeOpe]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqLotVarPdv]  DEFAULT ((0)) FOR [Flg_BlqLotVarPdv]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_BlqIncImpPriDup]  DEFAULT ((0)) FOR [Flg_BlqIncImpPriDup]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_GerPdvLotVar]  DEFAULT ((0)) FOR [Flg_GerPdvLotVar]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  DEFAULT ((0)) FOR [Flg_NaoBlqFinPdv]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  DEFAULT ((0)) FOR [Flg_NaoBlqDocPdv]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Flg_DscIcmDesoneNotFis]  DEFAULT ((0)) FOR [Flg_DscIcmDesoneNotFis]
GO

ALTER TABLE [dbo].[R_CLIEN] ADD  CONSTRAINT [DF_R_CLIEN_Id_Pais]  DEFAULT ((0)) FOR [Id_Pais]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_AGCOB] FOREIGN KEY([_Cod_Agente])
REFERENCES [dbo].[R_AGCOB] ([Codigo])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_AGCOB]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_BAIRR] FOREIGN KEY([Cod_Estado], [Cod_Cidade], [Cod_Bairro])
REFERENCES [dbo].[R_BAIRR] ([Cod_Estado], [Cod_Cidade], [Codigo])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_BAIRR]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_CIDAD] FOREIGN KEY([Cod_Estado], [Cod_Cidade])
REFERENCES [dbo].[R_CIDAD] ([Cod_Estado], [Codigo])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_CIDAD]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_CLCLI] FOREIGN KEY([Cod_ClaCli])
REFERENCES [dbo].[CLCLI] ([Codigo])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_CLCLI]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_ESTAD] FOREIGN KEY([Cod_Estado])
REFERENCES [dbo].[R_ESTAD] ([Codigo])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_ESTAD]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_INFCR] FOREIGN KEY([Cod_InfCredito])
REFERENCES [dbo].[INFCR] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_INFCR]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_RGTRI] FOREIGN KEY([_Cod_RegTri])
REFERENCES [dbo].[RGTRI] ([Cod_RegTri])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_RGTRI]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_RMATV] FOREIGN KEY([Cod_RamoAtividade])
REFERENCES [dbo].[RMATV] ([Codigo])
ON UPDATE CASCADE
ON DELETE CASCADE
NOT FOR REPLICATION 
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_RMATV]
GO

ALTER TABLE [dbo].[R_CLIEN]  WITH NOCHECK ADD  CONSTRAINT [FK_R_CLIEN_R_VENDE] FOREIGN KEY([_Cod_Vendedor])
REFERENCES [dbo].[VENDE] ([Codigo])
GO

ALTER TABLE [dbo].[R_CLIEN] CHECK CONSTRAINT [FK_R_CLIEN_R_VENDE]
GO


