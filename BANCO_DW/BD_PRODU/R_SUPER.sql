USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_SUPER]    Script Date: 28/06/2024 10:20:46 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_SUPER](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](30) NOT NULL,
	[Cod_Gerencia] [int] NULL,
	[Per_ComNor] [numeric](18, 4) NULL,
	[Per_ComAtoVen] [numeric](18, 4) NULL,
	[Per_ComAtoCob] [numeric](18, 4) NULL,
	[Dat_SldVrbIni] [smalldatetime] NULL,
	[Val_SldVrbIni] [numeric](18, 4) NULL,
	[Isn_CtaFin] [int] NULL,
	[Flg_BlqDebVrbSup] [bit] NULL,
	[Cpf] [varchar](11) NULL,
	[Email] [varchar](120) NULL,
	[Nome_Completo] [varchar](60) NULL,
	[Cod_Sex] [varchar](1) NULL,
	[Cod_EstCiv] [varchar](1) NULL,
	[Endereco] [varchar](30) NULL,
	[Bairro] [varchar](20) NULL,
	[Cep] [varchar](8) NULL,
	[Cidade] [varchar](20) NULL,
	[Estado] [varchar](2) NULL,
	[Fone] [varchar](20) NULL,
	[Celular] [varchar](20) NULL,
	[Observacao] [text] NULL,
	[Data_Admissao] [smalldatetime] NULL,
	[Data_Saida] [smalldatetime] NULL,
	[Bloqueado] [bit] NULL,
	[Razao_Social] [varchar](35) NULL,
	[Cgc] [varchar](14) NULL,
	[Cgf] [varchar](15) NULL,
	[Endereco_Com] [varchar](35) NULL,
	[Bairro_Com] [varchar](20) NULL,
	[Cep_Com] [varchar](8) NULL,
	[Cidade_Com] [varchar](20) NULL,
	[Estado_Com] [varchar](2) NULL,
	[Des_Favore] [varchar](40) NULL,
	[Cod_Bco] [smallint] NULL,
	[Num_Agenci] [varchar](6) NULL,
	[Num_Conta] [varchar](10) NULL,
 CONSTRAINT [PK_R_SUPER] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Cod_Gerencia]  DEFAULT (0) FOR [Cod_Gerencia]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Per_ComNor]  DEFAULT ((0)) FOR [Per_ComNor]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Per_ComAtoVen]  DEFAULT ((0)) FOR [Per_ComAtoVen]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Per_ComAtoCob]  DEFAULT ((0)) FOR [Per_ComAtoCob]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Val_SldVrbIni]  DEFAULT ((0)) FOR [Val_SldVrbIni]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Isn_CtaFin]  DEFAULT ((0)) FOR [Isn_CtaFin]
GO

ALTER TABLE [dbo].[R_SUPER] ADD  CONSTRAINT [DF_R_SUPER_Flg_BlqDebVrbSup]  DEFAULT ((0)) FOR [Flg_BlqDebVrbSup]
GO

ALTER TABLE [dbo].[R_SUPER]  WITH NOCHECK ADD  CONSTRAINT [FK_R_SUPER_R_GEREN] FOREIGN KEY([Cod_Gerencia])
REFERENCES [dbo].[R_GEREN] ([Codigo])
GO

ALTER TABLE [dbo].[R_SUPER] CHECK CONSTRAINT [FK_R_SUPER_R_GEREN]
GO


