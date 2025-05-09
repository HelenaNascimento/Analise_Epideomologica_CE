USE BD_DW
GO

/****** Object:  Table [dbo].[CLASS]    Script Date: 01/07/2024 16:08:12 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_CLASS](
	[Codigo] [varchar](7) NOT NULL,
	[Descricao] [varchar](25) NULL,
	[Nivel] [int] NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Flg_BlqCtrDocClaPrd] [bit] NULL,
	[Cod_EnqIpi] [varchar](3) NULL)
