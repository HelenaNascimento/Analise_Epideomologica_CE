USE [DW_PROD]
GO

/****** Object:  Table [dbo].[PDVCB_CP]    Script Date: 13/04/2024 17:26:33 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[PDVCB_CP](
	[Numero] [int] NOT NULL,
	[Cod_PedCli] [nchar](20) NULL,
	[Tip_Pedido] [varchar](1) NULL,
	[Cod_Cliente] [int] NULL,
	[Cod_RegTri] [int] NULL,
	[Cod_Vendedor] [int] NULL,
	[Id_PolCom] [int] NULL,
	[Cod_Prz] [int] NULL,
	[Cod_Rota] [int] NULL,
	[Status1] [varchar](1) NULL,
	[Status2] [varchar](1) NULL,
	[Dat_Pedido] [smalldatetime] NULL,
	[Hor_ConferIni] [smalldatetime] NULL,
	[Hor_ConferFim] [smalldatetime] NULL,
	[Hor_DbqFin] [smalldatetime] NULL,
	[Hor_DbqLic] [smalldatetime] NULL,
	[Hor_DbqRnt] [smalldatetime] NULL,
	[Hor_Entrada] [smalldatetime] NULL,
	[Hor_Saida] [smalldatetime] NULL,
	[Hor_Liberacao] [smalldatetime] NULL,
	[Hor_Fatura] [smalldatetime] NULL,
	[Hor_Fechamento] [smalldatetime] NULL,
	[Hor_Cancel] [smalldatetime] NULL,
	[Hor_ImpEtq] [smalldatetime] NULL,
	[Hor_Prenota] [smalldatetime] NULL,
	[Cod_FunSeparador] [int] NULL,
	[Cod_FunEmbalador] [int] NULL,
	[Cod_FunConferidor] [int] NULL,
	[Cod_Digitador] [int] NULL,
	[Usuario] [varchar](15) NULL,
	[Nom_UsuCancel] [varchar](15) NULL,
	[Nom_UsuDesbloq] [varchar](15) NULL,
	[Nom_UsuDesbloqLic] [varchar](15) NULL,
	[Nom_UsuDesbloqRnt] [varchar](15) NULL,
	[Observacao] [text] NULL,
	[Obs_Padrao] [text] NULL,
	[Obs_NotFis] [text] NULL,
	[Obs_IntFecPdv] [text] NULL,
	[Obs_IntCtaRec] [text] NULL,
	[C_VlrPedido] [numeric](18, 4) NULL,
 CONSTRAINT [PK_PDVCB_CP] PRIMARY KEY CLUSTERED 
(
	[Numero] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO


