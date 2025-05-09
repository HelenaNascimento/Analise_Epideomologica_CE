USE [DW_PROD]
GO

/****** Object:  Table [dbo].[FAKE_FIN]    Script Date: 13/04/2024 17:18:47 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[C_FIN](
	[CODCLI] [int] NULL,
	[RZ_SC] [varchar](80) NULL,
	[GRPCL] [int] NULL,
	[DES_GP] [varchar](80) NULL,
	[D_ATRAS] [int] NULL,
	[VLR_PEN] [decimal](20, 2) NULL,
	[VLR_AB] [decimal](20, 2) NULL,
	[VLR_PG] [decimal](20, 2) NULL
) ON [PRIMARY]
GO


