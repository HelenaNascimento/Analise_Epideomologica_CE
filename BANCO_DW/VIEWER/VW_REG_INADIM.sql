USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_REG_INADIM]    Script Date: 13/04/2024 16:49:12 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE VIEW [dbo].[VW_REG_INADIM] AS 

SELECT
	VLR_UM = (SELECT 
		SUM(VLR_DOCUMENTO) 
	FROM PROD_2023.DBO.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao = getdate() - 1),

	VLR_DEZ = (SELECT 
		SUM(VLR_DOCUMENTO) 
	FROM PROD_2023.DBO.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao < getdate() - 1
		AND Dat_Emissao >= getdate() - 10),

	VLR_VINT = (SELECT 
			SUM(VLR_DOCUMENTO)
		FROM PROD_2023.DBO.CTREC
		WHERE Cod_Estabe = 1
			AND STATUS = 'A'
			AND Dat_Emissao < getdate() - 10 
			AND Dat_Emissao >=  getdate() - 30),


	VLR_TRINT = (SELECT 
		SUM(VLR_DOCUMENTO)
	FROM PROD_2023.dbo.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao < getdate() - 30 
		AND Dat_Emissao >= getdate() - 60),


	VLR_SESS = (SELECT 
		SUM(VLR_DOCUMENTO)
	FROM PROD_2023.dbo.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao < getdate() - 60 
		AND Dat_Emissao >= getdate() - 90),


	VLR_NOV = (SELECT 
		SUM(VLR_DOCUMENTO)
	FROM PROD_2023.dbo.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao < getdate() - 90 
		AND Dat_Emissao >= getdate() - 120),

	VLR_C_VINT = (SELECT 
		SUM(VLR_DOCUMENTO)
	FROM PROD_2023.dbo.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao < getdate() - 120
		AND Dat_Emissao >= getdate() - 180),

	VLR_C_OIT = (SELECT 
		SUM(VLR_DOCUMENTO)
	FROM PROD_2023.dbo.CTREC
	WHERE Cod_Estabe = 1
		AND STATUS = 'A'
		AND Dat_Emissao < getdate() - 180)
GO


