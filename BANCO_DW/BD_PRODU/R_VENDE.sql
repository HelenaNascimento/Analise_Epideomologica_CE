USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_VENDE]    Script Date: 28/06/2024 10:18:00 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_VENDE](
	[Codigo] [int] NOT NULL,
	[Nome_Guerra] [varchar](15) NULL,
	[Nome_Completo] [varchar](60) NULL,
	[Cod_Gerencia] [int] NULL,
	[Data_Admissao] [smalldatetime] NULL,
	[Data_Saida] [smalldatetime] NULL,
	[Area_Atuacao] [varchar](30) NULL,
	[Bloqueado] [bit] NULL,
	[Salario_Fixo] [numeric](18, 4) NULL,
	[Per_Poupanca] [numeric](18, 8) NULL,
	[Per_ComAtoVen] [numeric](18, 8) NULL,
	[Per_ComAtoCob] [numeric](18, 8) NULL,
	[Cod_TipVenBas] [char](3) NULL,
	[_Cod_TabCom] [int] NULL,
	[Vlr_Objetivo] [numeric](18, 4) NULL,
	[Cpf] [varchar](11) NULL,
	[Rg] [varchar](20) NULL,
	[Endereco] [varchar](35) NULL,
	[Bairro] [varchar](20) NULL,
	[Cep] [varchar](8) NULL,
	[Cidade] [varchar](20) NULL,
	[Estado] [char](2) NULL,
	[Fone] [varchar](20) NULL,
	[Celular] [varchar](20) NULL,
	[Razao_Social] [varchar](35) NULL,
	[Endereco_Com] [varchar](35) NULL,
	[Bairro_Com] [varchar](20) NULL,
	[Cep_Com] [varchar](8) NULL,
	[Cidade_Com] [varchar](20) NULL,
	[Estado_Com] [char](2) NULL,
	[Cgc] [varchar](14) NULL,
	[Cgf] [varchar](15) NULL,
	[Core] [varchar](15) NULL,
	[Observacao] [text] NULL,
	[Flg_Export] [bit] NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[Des_Usuario] [varchar](30) NULL,
	[Des_Senha] [varchar](30) NULL,
	[Des_ServSmtp] [varchar](30) NULL,
	[Des_ServPop] [varchar](30) NULL,
	[Num_TeleProv] [varchar](15) NULL,
	[Des_Email] [varchar](120) NULL,
	[Cod_Bco] [smallint] NULL,
	[Num_Agenci] [varchar](6) NULL,
	[Num_Conta] [varchar](10) NULL,
	[Des_Favore] [varchar](40) NULL,
	[Cod_RefPrati] [varchar](14) NULL,
	[Dat_SldVrbIni] [smalldatetime] NULL,
	[Val_SldVrbIni] [numeric](18, 4) NULL,
	[Cod_DDD] [varchar](2) NULL,
	[Cod_Supervisor] [int] NULL,
	[Isn_CtaFin] [int] NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Des_Nacionali] [varchar](20) NULL,
	[Cod_Sex] [varchar](1) NULL,
	[Cod_EstCiv] [varchar](1) NULL,
	[Des_OrgExpRg] [varchar](12) NULL,
	[Dat_EmiRg] [smalldatetime] NULL,
	[Versao] [bigint] NOT NULL,
	[Cod_EstabeOpe] [int] NULL,
	[Email] [varchar](120) NULL,
 CONSTRAINT [PK_R_VENDE] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Cod_Gerencia]  DEFAULT (0) FOR [Cod_Gerencia]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Bloqueado]  DEFAULT (0) FOR [Bloqueado]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Salario_Fixo]  DEFAULT (0) FOR [Salario_Fixo]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Per_Poupanca]  DEFAULT (0) FOR [Per_Poupanca]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Per_ComAtoVen]  DEFAULT (0) FOR [Per_ComAtoVen]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Per_ComAtoCob]  DEFAULT (0) FOR [Per_ComAtoCob]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Cod_TabCom]  DEFAULT (0) FOR [_Cod_TabCom]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Vlr_Objetivo]  DEFAULT (0) FOR [Vlr_Objetivo]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Flg_Export]  DEFAULT (0) FOR [Flg_Export]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Cod_Bco]  DEFAULT (0) FOR [Cod_Bco]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Val_SldVrbIni]  DEFAULT (0) FOR [Val_SldVrbIni]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Cod_Supervisor]  DEFAULT (0) FOR [Cod_Supervisor]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Isn_CtaFin]  DEFAULT ((0)) FOR [Isn_CtaFin]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Versao]  DEFAULT ((0)) FOR [Versao]
GO

ALTER TABLE [dbo].[R_VENDE] ADD  CONSTRAINT [DF_R_VENDE_Cod_EstabeOpe]  DEFAULT ((0)) FOR [Cod_EstabeOpe]
GO

ALTER TABLE [dbo].[R_VENDE]  WITH NOCHECK ADD  CONSTRAINT [FK_R_VENDE_R_GEREN] FOREIGN KEY([Cod_Gerencia])
REFERENCES [dbo].[GEREN] ([Codigo])
GO

ALTER TABLE [dbo].[R_VENDE] CHECK CONSTRAINT [FK_R_VENDE_R_GEREN]
GO

ALTER TABLE [dbo].[R_VENDE]  WITH NOCHECK ADD  CONSTRAINT [FK_R_VENDE_R_SUPER] FOREIGN KEY([Cod_Supervisor])
REFERENCES [dbo].[SUPER] ([Codigo])
GO

ALTER TABLE [dbo].[R_VENDE] CHECK CONSTRAINT [FK_R_VENDE_SUPER]
GO


