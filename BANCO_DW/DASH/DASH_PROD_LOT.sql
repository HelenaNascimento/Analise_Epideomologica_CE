USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_PROD_LOT]    Script Date: 11/04/2024 17:09:42 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE VIEW [dbo].[DASH_PROD_LOT] AS

SELECT 
	DISTINCT
	Cod_Fabricante,
	FB.Fantasia,
	pr.Codigo,
	Descri,
	PR.Cod_EAN,
	ES.Prc_CusMedCom,
	LOT.Cod_Lote,
	LOT.Dat_Fabric,
	LOT.Dat_Vencim,
	LOT.Qtd_Saldo
	FROM PROD_2023.dbo.PRODU PR
		inner join PROD_2023.dbo.PRLOT LOT ON PR.Codigo = LOT.Cod_Produt
		inner join PROD_2023.dbo.PRXES ES on pr.codigo = es.Cod_Produt and lot.cod_estabe = es.Cod_Estabe
	    left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.Codigo
	WHERE
	lot.Cod_Estabe = 1
	and pr.Tipo = 'R'
	--and PR.Cod_EAN LIKE'7%'
	and LOT.Qtd_Saldo > 0


GO


