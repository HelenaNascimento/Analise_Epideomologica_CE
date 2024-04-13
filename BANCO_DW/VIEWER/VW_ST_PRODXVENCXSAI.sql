USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_ST_PRODXVENCXSAI]    Script Date: 13/04/2024 16:50:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE VIEW [dbo].[VW_ST_PRODXVENCXSAI] AS
SELECT 
	DISTINCT
	IT.Cod_Produto,
	PR.Descri,
	FB.Fantasia,
	SUM(IT.Qtd_Produto) AS Qtd_Produto,
	CONVERT(DECIMAL(20,2) ,SUM(IT.Vlr_LiqItem)) AS Vlr_LiqItem,
	Mes=
	CASE
		WHEN MONTH(cb.Dat_Emissao) = '01' THEN 'Jan'
		WHEN MONTH(cb.Dat_Emissao) = '02' THEN 'Fev'
		WHEN MONTH(cb.Dat_Emissao) = '03' THEN 'Mar'
		WHEN MONTH(cb.Dat_Emissao) = '04' THEN 'Abr'
		WHEN MONTH(cb.Dat_Emissao) = '05' THEN 'Mai'
		WHEN MONTH(cb.Dat_Emissao) = '06' THEN 'Jun'
		WHEN MONTH(cb.Dat_Emissao) = '07' THEN 'Jul'
		WHEN MONTH(cb.Dat_Emissao) = '08' THEN 'Ago'
		WHEN MONTH(cb.Dat_Emissao) = '09' THEN 'Set'
		WHEN MONTH(cb.Dat_Emissao) = '10' THEN 'Out'
		WHEN MONTH(cb.Dat_Emissao) = '11' THEN 'Nov'
		WHEN MONTH(cb.Dat_Emissao) = '12' THEN 'Dez'
	END,
	ANO = 
		CASE 
			WHEN YEAR(cb.Dat_Emissao) = '2023' THEN '2023'
			WHEN YEAR(cb.Dat_Emissao) = '2024' THEN '2024'
		END
	FROM PROD_2023.dbo.NFSIT IT
		INNER JOIN PROD_2023.dbo.NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
		LEFT JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
		LEFT JOIN PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
WHERE 
	IT.Cod_Estabe = 1
AND IT.Cod_Cfo = 5949
AND CB.Ser_Nota = '1'
AND CB.Cod_Cliente = 12976
AND CB.Dat_Emissao >= '20230101'
GROUP BY 	
	IT.Cod_Produto,
	PR.Descri,
	Dat_Emissao, 
	FB.Fantasia
GO


