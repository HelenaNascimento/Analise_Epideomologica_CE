USE [PROD_2023]
GO

/****** Object:  Table [dbo].[R_RGTRI]    Script Date: 28/06/2024 10:24:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_RGTRI](
	[Cod_RegTri] [int] NOT NULL,
	[Des_RegTri] [varchar](30) NULL,
	[Ctrl_ImpIcmTotNfs] [bit] NULL,
	[Flg_ConsumFin] [bit] NULL,
	[Msg_Nf] [varchar](60) NULL,
	[Flg_RegEspTrb] [bit] NULL,
	[Flg_DscIcmOrgPub] [bit] NULL,
	[Flg_DscIcmOrgPubAcr] [bit] NULL,
	[CodAnt] [int] NULL,
	[NovoCodigo] [int] NULL,
	[Flg_UsaCfoSbtSaiIntGenSim] [bit] NULL,
	[Flg_UsaCrePresum] [bit] NULL,
	[Flg_UsaDebPresum] [bit] NULL,
	[Flg_DatEmiPrdCip] [bit] NULL,
	[Flg_DatAntUltAtuPrdCip] [bit] NULL,
	[Cod_Canal] [varchar](5) NULL,
	[Id_MensagReg] [int] NULL,
	[Id_MsgReg2] [int] NULL,
	[Flg_DscSbtOrgPub] [bit] NULL,
	[Flg_SujAntIcm] [bit] NULL)