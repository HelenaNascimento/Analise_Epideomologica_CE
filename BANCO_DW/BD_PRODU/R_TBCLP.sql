USE BD_DW
GO

/****** Object:  Table [dbo].[TBCLP]    Script Date: 01/07/2024 17:02:30 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_TBCLP](
	[Cod_Layout] [int] NOT NULL,
	[Des_Layout] [varchar](25) NOT NULL,
	[Des_DirPed] [varchar](100) NULL,
	[Des_DirFal] [varchar](100) NULL,
	[Des_DirNot] [varchar](100) NULL,
	[Qtd_Interv] [int] NULL,
	[Flg_RetFal] [bit] NOT NULL,
	[Flg_RetNot] [bit] NOT NULL,
	[Cod_Empres] [varchar](10) NULL,
	[Des_DirCot] [varchar](100) NULL,
	[Des_DirRetCot] [varchar](100) NULL,
	[Des_EnderecoFtp] [varchar](256) NULL,
	[Des_UsuarioFtp] [varchar](40) NULL,
	[Des_SenhaFtp] [varchar](30) NULL,
	[Flg_ExpNotXml] [bit] NULL,
	[Des_DirTit] [varchar](100) NULL,
	[Flg_FecAutPdv] [bit] NULL,
	[Flg_BlqDscAutIte] [bit] NULL,
	[Des_DirDev] [varchar](100) NULL,
	[Flg_UsaOpeCli] [bit] NULL,
	[Cod_TipRetMtvRejCab] [varchar](1) NULL,
	[Cod_TipRetMtvRejIte] [varchar](1) NULL,
	[Flg_UsaNovoPDE] [bit] NULL,
	[Per_MidPad] [numeric](8, 4) NOT NULL,
	[Tip_Priori] [varchar](12) NULL,
 CONSTRAINT [PK_R_TBCLP] PRIMARY KEY CLUSTERED 
(
	[Cod_Layout] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Qtd_Interv]  DEFAULT (0) FOR [Qtd_Interv]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Flg_RetFal]  DEFAULT (0) FOR [Flg_RetFal]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Flg_RetNot]  DEFAULT (0) FOR [Flg_RetNot]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Flg_ExpNotXml]  DEFAULT (0) FOR [Flg_ExpNotXml]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Flg_FecAutPdv]  DEFAULT ((0)) FOR [Flg_FecAutPdv]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Flg_BlqDscAutIte]  DEFAULT ((0)) FOR [Flg_BlqDscAutIte]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  CONSTRAINT [DF_TBCLP_Flg_UsaOpeCli]  DEFAULT ((0)) FOR [Flg_UsaOpeCli]
GO

ALTER TABLE [dbo].[R_TBCLP] ADD  DEFAULT ((0)) FOR [Per_MidPad]
GO


