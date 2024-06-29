USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_RMATV]    Script Date: 28/06/2024 10:23:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_RMATV](
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](30) NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Flg_RedAlqVenPisCof] [bit] NULL,
	[Flg_BlqCtrLicSauVen] [bit] NULL,
	[Flg_BlqPdvLicAnvVen] [bit] NULL,
	[Flg_BlqPdvLicCrfVen] [bit] NULL,
	[Flg_BlqPdvAlvFunVen] [bit] NULL,
	[Flg_BlqCriLicCot] [bit] NULL,
	[Cod_EnqIpi] [varchar](3) NULL,
	[Flg_BlqPdvLicVen] [bit] NULL,
 CONSTRAINT [PK_R_RMATV] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_RedAlqVenPisCof]  DEFAULT ((0)) FOR [Flg_RedAlqVenPisCof]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_BlqCtrLicSauVen]  DEFAULT ((0)) FOR [Flg_BlqCtrLicSauVen]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_BlqPdvLicAnvVen]  DEFAULT ((0)) FOR [Flg_BlqPdvLicAnvVen]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_BlqPdvLicCrfVen]  DEFAULT ((0)) FOR [Flg_BlqPdvLicCrfVen]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_BlqPdvAlvFunVen]  DEFAULT ((0)) FOR [Flg_BlqPdvAlvFunVen]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_BlqCriLicCot]  DEFAULT ((0)) FOR [Flg_BlqCriLicCot]
GO

ALTER TABLE [dbo].[R_RMATV] ADD  CONSTRAINT [DF_R_RMATV_Flg_BlqPdvLicVen]  DEFAULT ((0)) FOR [Flg_BlqPdvLicVen]
GO


