USE [DW_PROD]
GO

/****** Object:  Table [dbo].[DCIMP]    Script Date: 13/04/2024 17:12:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DCIMP](
	[Id_DclImp] [int] NOT NULL,
	[Num_DocImp] [varchar](12) NOT NULL,
	[Dat_RegDoc] [smalldatetime] NULL,
	[Des_LocDesemb] [varchar](60) NULL,
	[UF_Desemb] [varchar](2) NULL,
	[Dat_Desemb] [smalldatetime] NULL,
	[Cod_TipViaTransp] [smallint] NULL,
	[Vlr_AFRMM] [numeric](18, 4) NULL,
	[Cod_TipIntermedi] [smallint] NULL,
	[Num_CNPJAdquirente] [varchar](14) NULL,
	[UF_Adquirente] [varchar](2) NULL,
	[Cod_Exportador] [varchar](60) NULL,
	[Cod_Fornec] [int] NULL,
	[Qtd_Adicao] [smallint] NULL,
	[Qtd_PesBru] [numeric](18, 5) NULL,
	[Qtd_PesLiq] [numeric](18, 5) NULL,
	[Vlr_VMLE] [numeric](18, 4) NULL,
	[Vlr_Frete] [numeric](18, 4) NULL,
	[Vlr_Seguro] [numeric](18, 4) NULL,
	[Vlr_VMLD] [numeric](18, 4) NULL,
	[Vlr_II] [numeric](18, 4) NULL,
	[Vlr_Ipi] [numeric](18, 4) NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[Vlr_DirAntDump] [numeric](18, 4) NULL,
	[Vlr_TaxSiscomex] [numeric](18, 4) NULL,
	[Vlr_OutDespes] [numeric](18, 4) NULL,
 CONSTRAINT [PK_DCIMP] PRIMARY KEY CLUSTERED 
(
	[Id_DclImp] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Cod_Fornec]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Qtd_Adicao]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Qtd_PesBru]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Qtd_PesLiq]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_VMLE]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_Frete]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_Seguro]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_VMLD]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_II]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_Ipi]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_DirAntDump]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_TaxSiscomex]
GO

ALTER TABLE [dbo].[DCIMP] ADD  DEFAULT ((0)) FOR [Vlr_OutDespes]
GO


