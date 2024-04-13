USE [DW_PROD]
GO

/****** Object:  Table [dbo].[COMPR]    Script Date: 13/04/2024 17:07:16 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[COMPR](
	[Codigo] [int] NOT NULL,
	[Nome_Guerra] [varchar](20) NULL,
	[Nome_Completo] [varchar](60) NULL,
	[Cpf] [varchar](11) NULL,
	[Rg] [varchar](20) NULL,
	[Des_OrgExpRg] [varchar](12) NULL,
	[Dat_EmiRg] [smalldatetime] NULL,
	[Endereco] [varchar](35) NULL,
	[Bairro] [varchar](20) NULL,
	[Cep] [varchar](8) NULL,
	[Cidade] [varchar](20) NULL,
	[Estado] [char](2) NULL,
	[Fone] [varchar](20) NULL,
	[Celular] [varchar](20) NULL,
	[Email] [varchar](120) NULL,
	[Des_Nacionali] [varchar](20) NULL,
	[Cod_Sex] [varchar](1) NULL,
	[Cod_EstCiv] [varchar](1) NULL,
	[Data_Admissao] [smalldatetime] NULL,
	[Data_Saida] [smalldatetime] NULL,
	[Usuario] [varchar](35) NULL,
	[Transacao] [smalldatetime] NULL,
	[Flg_Bloqueado] [bit] NULL,
 CONSTRAINT [PK_COMPR] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[COMPR] ADD  CONSTRAINT [DF_COMPR_Flg_Bloqueado]  DEFAULT ((0)) FOR [Flg_Bloqueado]
GO


