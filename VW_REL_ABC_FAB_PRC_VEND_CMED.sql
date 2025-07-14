USE [DMD_TESTE]
GO

/****** Object:  View [dbo].[VW_REL_ABC_FAB_PRC_VEND_CMED]    Script Date: 01/07/2025 09:38:38 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[VW_REL_ABC_FAB_PRC_VEND_CMED] AS 

SELECT
	FB.Fantasia AS Fabricante,
	PR.Codigo AS Codigo,
	PR.Descri AS Produto,
	Format(Vlr_PrcVen , 'c', 'pt-br') AS Vlr_PrcVen,
	Format(Vlr_CusMedCom, 'c', 'pt-br') AS Vlr_CusMedCom,
	YEAR(Dat_Alteracao) AS Ano_Alteracao,
	MONTH(HS.Dat_Alteracao) AS Mes_Alteracao,
	DAY(HS.Dat_Alteracao) AS Dia_Alteracao
FROM 
		(SELECT	DISTINCT
				COD_ESTABE, 
				COD_PRODUTO, 
				Vlr_PrcVen, 
				Vlr_CusMedCom, 
				MAX(Dat_Alteracao) AS Dat_Alteracao 
				FROM HSPRC 
			WHERE Cod_Estabe = 1 
				and YEAR(Dat_Alteracao) > YEAR(GETDATE()) -5
				and MONTH(Dat_Alteracao) <= MONTH(GETDATE()) -1
			GROUP BY 
					COD_ESTABE, 
					Cod_Produto, 
					Vlr_PrcVen, 
					Vlr_CusMedCom) HS
	JOIN PRODU PR ON HS.Cod_Produto = PR.Codigo
	JOIN FABRI FB ON PR.Cod_Fabricante = FB.Codigo
WHERE PR.Flag_ImprClassif1 <>'N'

GO


