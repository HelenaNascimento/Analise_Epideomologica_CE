USE [DW_PROD]
GO

/****** Object:  Table [dbo].[PDVCB_CP]    Script Date: 13/04/2024 17:26:33 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_PDVCB](
	[Numero] [int] NOT NULL,
	[Cod_PedCli] [nchar](20) ,
	[Tip_Pedido] [varchar](1) ,
	[Cod_Cliente] [int] ,
	[Cod_RegTri] [int] ,
	[Cod_Vendedor] [int] ,
	[Id_PolCom] [int] ,
	[Cod_Prz] [int] ,
	[Cod_Rota] [int] ,
	[Status1] [varchar](1) ,
	[Status2] [varchar](1) ,
	[Dat_Pedido] [smalldatetime] ,
	[Hor_ConferIni] [smalldatetime] ,
	[Hor_ConferFim] [smalldatetime] ,
	[Hor_DbqFin] [smalldatetime] ,
	[Hor_DbqLic] [smalldatetime] ,
	[Hor_DbqRnt] [smalldatetime] ,
	[Hor_Entrada] [smalldatetime] ,
	[Hor_Saida] [smalldatetime] ,
	[Hor_Liberacao] [smalldatetime] ,
	[Hor_Fatura] [smalldatetime] ,
	[Hor_Fechamento] [smalldatetime] ,
	[Hor_Cancel] [smalldatetime] ,
	[Hor_ImpEtq] [smalldatetime] ,
	[Hor_Prenota] [smalldatetime] ,
	[Cod_FunSeparador] [int] ,
	[Cod_FunEmbalador] [int] ,
	[Cod_FunConferidor] [int] ,
	[Cod_Digitador] [int] ,
	[Usuario] [varchar](15) ,
	[Nom_UsuCancel] [varchar](15) ,
	[Nom_UsuDesbloq] [varchar](15),
	[Nom_UsuDesbloqLic] [varchar](15) ,
	[Nom_UsuDesbloqRnt] [varchar](15) ,
	[Observacao] [text] ,
	[Obs_Padrao] [text] ,
	[Obs_NotFis] [text] ,
	[Obs_IntFecPdv] [text] ,
	[Obs_IntCtaRec] [text] ,
	[C_VlrPedido] [numeric](18, 4) ,
 CONSTRAINT [PK_PDVCB_CP] PRIMARY KEY CLUSTERED 
(
	[Numero] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO


