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
	[Flg_SujAntIcm] [bit] NULL,
 CONSTRAINT [PK_R_RGTRI] PRIMARY KEY CLUSTERED 
(
	[Cod_RegTri] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Ctrl_ImpIcmTotNfs]  DEFAULT (0) FOR [Ctrl_ImpIcmTotNfs]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Flg_ConsumFin]  DEFAULT (0) FOR [Flg_ConsumFin]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Flg_RegEspTrb]  DEFAULT (0) FOR [Flg_RegEspTrb]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Flg_DscIcmOrgPub]  DEFAULT (0) FOR [Flg_DscIcmOrgPub]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Flg_DscIcmOrgPubAcr]  DEFAULT (0) FOR [Flg_DscIcmOrgPubAcr]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Flg_UsaCrePresum]  DEFAULT ((0)) FOR [Flg_UsaCrePresum]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Flg_UsaDebPresum]  DEFAULT ((0)) FOR [Flg_UsaDebPresum]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  DEFAULT ((0)) FOR [Flg_DatEmiPrdCip]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  DEFAULT ((0)) FOR [Flg_DatAntUltAtuPrdCip]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Id_MensagReg]  DEFAULT ((0)) FOR [Id_MensagReg]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  CONSTRAINT [DF_R_RGTRI_Id_MsgReg2]  DEFAULT ((0)) FOR [Id_MsgReg2]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  DEFAULT ((0)) FOR [Flg_DscSbtOrgPub]
GO

ALTER TABLE [dbo].[R_RGTRI] ADD  DEFAULT ((0)) FOR [Flg_SujAntIcm]
GO


