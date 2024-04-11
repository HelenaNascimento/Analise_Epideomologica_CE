USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_PROD_LTL]    Script Date: 11/04/2024 17:10:50 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[DASH_PROD_LTL] AS

SELECT 
	DISTINCT
	Cod_Fabricante,
	FB.Fantasia,
	pr.Codigo,
	Descri,
	ES.Prc_CusMedCom,
    ltl.Num_Rua,
    ltl.Num_Col,
    ltl.Num_Niv,
    ltl.Num_Apt,
   LTL.Cod_Lote as Pulmao,
	LTL.Dat_Fabric,
   LTL.Dat_Vencim,
   ltl.Qtd_Saldo as Qtd_Saldo
	FROM PROD_2023.dbo.PRODU PR
		inner join PROD_2023.dbo.PRLOT LOT ON PR.Codigo = LOT.Cod_Produt
		inner join PROD_2023.dbo.PRXES ES on pr.codigo = es.Cod_Produt and lot.cod_estabe = es.Cod_Estabe
        inner join PROD_2023.dbo.PRLTL LTL on pr.Codigo = LTL.Cod_Produt and es.Cod_Estabe = ltl.Cod_Estabe
	    left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.Codigo
	WHERE
	lot.Cod_Estabe = 1
	and LOT.Qtd_Saldo > 0
GO


