USE [DW_PROD]
GO

/****** Object:  Table [dbo].[DIADI]    Script Date: 13/04/2024 17:15:29 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DIADI](
	[Id_DclImp] [int] NOT NULL,
	[Num_Adicao] [smallint] NOT NULL,
	[Num_SeqIte] [smallint] NOT NULL,
	[Des_Mercad] [varchar](400) NULL,
	[Cod_Lote] [varchar](20) NULL,
	[Dat_Fabric] [smalldatetime] NULL,
	[Dat_Vencim] [smalldatetime] NULL,
	[Qtd_PesLiq] [numeric](18, 5) NULL,
	[Qtd_Und] [numeric](18, 5) NULL,
	[Vlr_MerUndCom] [numeric](18, 5) NULL,
	[Vlr_FreRat] [numeric](18, 4) NULL,
	[Vlr_SegRat] [numeric](18, 4) NULL,
	[Vlr_II] [numeric](18, 4) NULL,
	[Alq_Ipi] [numeric](7, 4) NULL,
	[Vlr_IPI] [numeric](18, 4) NULL,
	[Vlr_BasCalPisCofins] [numeric](18, 4) NULL,
	[Per_RedBasCalPisCof] [numeric](7, 4) NULL,
	[Alq_Pis] [numeric](7, 4) NULL,
	[Vlr_Pis] [numeric](18, 4) NULL,
	[Alq_Cofins] [numeric](7, 4) NULL,
	[Vlr_Cofins] [numeric](18, 4) NULL,
	[Alq_Icm] [numeric](7, 4) NULL,
 CONSTRAINT [PK_DIADI] PRIMARY KEY CLUSTERED 
(
	[Id_DclImp] ASC,
	[Num_Adicao] ASC,
	[Num_SeqIte] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Qtd_PesLiq]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Qtd_Und]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_MerUndCom]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_FreRat]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_SegRat]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_II]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Alq_Ipi]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_IPI]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_BasCalPisCofins]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Per_RedBasCalPisCof]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Alq_Pis]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_Pis]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Alq_Cofins]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Vlr_Cofins]
GO

ALTER TABLE [dbo].[DIADI] ADD  DEFAULT ((0)) FOR [Alq_Icm]
GO

ALTER TABLE [dbo].[DIADI]  WITH CHECK ADD  CONSTRAINT [FK_DIADI_DIADC] FOREIGN KEY([Id_DclImp], [Num_Adicao])
REFERENCES [dbo].[DIADC] ([Id_DclImp], [Num_Adicao])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[DIADI] CHECK CONSTRAINT [FK_DIADI_DIADC]
GO


