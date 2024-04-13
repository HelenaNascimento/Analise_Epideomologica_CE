USE [DW_PROD]
GO

/****** Object:  Table [dbo].[FS_F100]    Script Date: 13/04/2024 17:25:22 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[FS_F100](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Tip_Regist] [int] NOT NULL,
	[Num_CnpjCpf] [varchar](14) NOT NULL,
	[Cod_Produt] [int] NULL,
	[Dat_Emissa] [datetime] NOT NULL,
	[Vlr_Operac] [numeric](18, 4) NULL,
	[Cod_NatBasCre] [char](2) NULL,
	[Cod_OriCred] [int] NOT NULL,
	[CST_Pis] [char](2) NULL,
	[Vlr_BasPis] [numeric](18, 4) NULL,
	[Alq_Pis] [numeric](18, 4) NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[CST_Cofins] [char](2) NULL,
	[Vlr_BasCof] [numeric](18, 4) NULL,
	[Alq_Cof] [numeric](18, 4) NULL,
	[Vlr_Cof] [numeric](18, 4) NULL,
	[Cod_CtaCtb] [varchar](100) NULL,
	[Cod_CntRes] [varchar](100) NULL,
	[Descricao] [varchar](100) NULL,
	[Dat_Importa] [datetime] NULL,
	[Transacao] [datetime] NULL,
	[Usuario] [varchar](15) NULL,
PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Vlr_Operac]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Vlr_BasPis]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Alq_Pis]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Vlr_BasCof]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Alq_Cof]
GO

ALTER TABLE [dbo].[FS_F100] ADD  DEFAULT ((0)) FOR [Vlr_Cof]
GO


