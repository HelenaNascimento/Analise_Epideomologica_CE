USE BD_DW
GO

/****** Object:  Table [dbo].[CIDAD]    Script Date: 28/06/2024 09:34:57 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_CIDAD](
	[Cod_Estado] [char](2) NOT NULL,
	[Codigo] [int] NOT NULL,
	[Descricao] [varchar](25) NULL,
	[Codigo_BMS] [varchar](12) NULL,
	[Nivel_ComVen] [char](1) NULL,
	[Cod_CidIbge] [varchar](7) NULL,
	[Flg_Excluido] [bit] NULL)
