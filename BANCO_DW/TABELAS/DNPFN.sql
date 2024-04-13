USE [DW_PROD]
GO

/****** Object:  Table [dbo].[DNPFN]    Script Date: 13/04/2024 17:17:38 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DNPFN](
	[Cod_Estabe] [int] NOT NULL,
	[Protocolo] [int] NOT NULL,
	[Cod_Pedido] [int] NOT NULL,
	[Qtd_ParPed] [int] NULL,
	[Qtd_PrzPed] [int] NULL,
	[Qtd_ParNfe] [int] NULL,
	[Qtd_PrzNfe] [int] NULL,
	[Status] [char](1) NULL,
	[Usuario] [varchar](15) NULL,
	[Transacao] [datetime] NULL,
	[Des_Just] [varchar](200) NULL,
	[Dat_Autori] [datetime] NULL,
 CONSTRAINT [PK_DNPFN] PRIMARY KEY CLUSTERED 
(
	[Cod_Estabe] ASC,
	[Protocolo] ASC,
	[Cod_Pedido] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO


