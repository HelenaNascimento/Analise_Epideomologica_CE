USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_MOVEST]    Script Date: 13/04/2024 16:27:55 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_MOVEST] AS
SELECT	
	DISTINCT
	SL.Cod_Produt,
	Dat_Movime,
	LT.Cod_Lote,
	LT.Dat_Fabric,
	LT.Dat_Vencim,
	Qtd_EntCom,
	Qtd_EntTrf,
	Qtd_EntDev,
	Qtd_EntOut,
	Qtd_SaiVen,
	Qtd_SaiTrf,
	Qtd_SaiOut,
	Qtd_SldAtu,
	SL.Qtd_SldPra,
	SL.Qtd_SldDep
	FROM PROD_2023.dbo.PRSLD SL
	 INNER JOIN PROD_2023.dbo.PRLOT LT
		ON SL.Cod_Estabe = LT.Cod_Estabe AND SL.Cod_Produt = LT.Cod_Produt 
WHERE SL.Cod_Estabe = 1
	AND SL.Dat_Movime >= '20220101'
	AND LT.Qtd_Fisico > 0
GO


