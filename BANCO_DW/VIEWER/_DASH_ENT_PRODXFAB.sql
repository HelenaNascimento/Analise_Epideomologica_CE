USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_ENT_PRODXFAB]    Script Date: 27/06/2024 13:23:29 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE VIEW [dbo].[DASH_ENT_PRODXFAB] AS
SELECT
	Cod_Fabricante as Codigo,
	FB.Fantasia as Fantasia,

	sum(it.Qtd_PedFat) as Qtd_Compra,
	sum((Prc_Unitario * it.Qtd_PedFat)) as Valor,
	month(CB.DAT_ENTRADA) as Mes,
	year(CB.DAT_ENTRADA) as Ano
	FROM PROD_2023.dbo.NFEIT IT
		INNER JOIN PROD_2023.dbo.NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
		INNER JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
		left join PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
WHERE 
IT.Cod_Estabe = 1
AND year(CB.DAT_ENTRADA) >= year(getdate()) -1
AND IT.Cod_Cfo in (1910, 2910, 2102, 2403, 2404) 

GROUP BY
	Cod_Fabricante,
	FB.Fantasia,
	CB.DAT_ENTRADA

GO


