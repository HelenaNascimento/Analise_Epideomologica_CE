USE [DW_PROD]
GO

/****** Object:  View [dbo].[ENT_PROD_BON]    Script Date: 11/04/2024 17:17:33 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


create view [dbo].[ENT_PROD_BON] as
SELECT
	distinct
	FB.Codigo,
	FB.Fantasia,
	QT.QTD_Compra,
	Vlr.Valor,
	month(CB.DAT_ENTRADA) as Mes,
	year(CB.DAT_ENTRADA) as Ano
	FROM PROD_2023.dbo.NFEIT IT
		INNER JOIN PROD_2023.dbo.NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
		INNER JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
		left join PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
		left join (	SELECT
						Cod_Fabricante,
						FB.Fantasia,
						SUM(IT.Qtd_PedFat) AS QTD_Compra
						FROM PROD_2023.dbo.NFEIT IT
							INNER JOIN PROD_2023.dbo.NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
							INNER JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
							left join PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
					WHERE 
					IT.Cod_Estabe = 1
					AND Cod_Fabricante > 0
					AND IT.Cod_Cfo in (1910, 2910) 
					AND year(CB.DAT_ENTRADA) >= year(getdate()) -1
					AND year(CB.DAT_ENTRADA) <= year(getdate()) 
 
					group by 
						Cod_Fabricante,
						FB.Fantasia
						) QT ON FB.Codigo = QT.Cod_Fabricante
		left join (
					SELECT
						distinct
						Cod_Fabricante,
						FB.Fantasia,	
						sum((Prc_Unitario * it.Qtd_PedFat)) as Valor
						FROM PROD_2023.dbo.NFEIT IT
							INNER JOIN PROD_2023.dbo.NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
							INNER JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
							left join PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
					WHERE 
					IT.Cod_Estabe = 1
					AND Cod_Fabricante > 0
					AND IT.Cod_Cfo in (1910, 2910) 
					AND year(CB.DAT_ENTRADA) >= year(getdate()) -1
					AND year(CB.DAT_ENTRADA) <= year(getdate()) 

					GROUP BY
						Cod_Fabricante,
						FB.Fantasia ) Vlr ON FB.Codigo = Vlr.Cod_Fabricante
WHERE 
	IT.Cod_Estabe = 1
	AND FB.Codigo > 0
	AND IT.Cod_Cfo in (1910, 2910) 
	AND year(CB.DAT_ENTRADA) >= year(getdate()) -1
	AND year(CB.DAT_ENTRADA) <= year(getdate()) 
GO


