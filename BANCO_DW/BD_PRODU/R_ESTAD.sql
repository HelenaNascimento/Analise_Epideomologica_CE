USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_ESTAD]    Script Date: 28/06/2024 10:30:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_ESTAD](
	[Codigo] [char](2) NOT NULL,
	[Descricao] [varchar](20) NULL,
	[_Credenciamento] [varchar](15) NULL,
	[_Flg_RegSubsTrib] [bit] NULL,
	[Per_DscTri] [numeric](18, 8) NULL,
	[ObservacaoNF] [varchar](80) NULL,
	[Obs2_NF] [varchar](80) NULL,
	[Obs3_NF] [varchar](80) NULL,
	[Cod_Ibge] [char](2) NULL,
 CONSTRAINT [PK_R_ESTAD] PRIMARY KEY CLUSTERED 
(
	[Codigo] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO


