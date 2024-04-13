USE [DW_PROD]
GO

/****** Object:  Table [dbo].[DNCIE]    Script Date: 13/04/2024 17:16:29 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DNCIE](
	[Cod_Dncie] [int] IDENTITY(1,1) NOT NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Protocolo] [int] NOT NULL,
	[Ean_Produt] [char](128) NULL,
	[Cod_Produt] [int] NOT NULL,
	[Status] [char](1) NOT NULL,
	[Qtd_Nota] [int] NULL,
	[Qtd_Coleta] [int] NULL,
	[Qtd_caixa] [int] NULL,
	[Flg_Autoriz] [bit] NULL,
	[Des_Just] [varchar](100) NULL,
	[Cod_Lote] [varchar](20) NOT NULL,
	[Usuario] [varchar](15) NULL,
	[Transacao] [smalldatetime] NULL,
	[Qtd_EmbFec] [int] NOT NULL,
	[Qtd_Avaria] [int] NULL,
	[Cod_Confer] [smallint] NOT NULL,
	[Dat_Fabric] [datetime] NULL,
	[Dat_Vencim] [datetime] NULL,
	[Qtd_Faltas] [int] NULL,
	[Dat_IniCol] [datetime] NULL,
	[Dat_FimCol] [datetime] NULL,
 CONSTRAINT [PK_DNCIE] PRIMARY KEY CLUSTERED 
(
	[Cod_Dncie] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO


