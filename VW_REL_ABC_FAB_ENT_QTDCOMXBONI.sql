USE [DMD_TESTE]
GO

/****** Object:  View [dbo].[VW_REL_ABC_FAB_ENT_QTDCOMXBONI]    Script Date: 01/07/2025 09:38:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[VW_REL_ABC_FAB_ENT_QTDCOMXBONI] AS


with COMPRA AS  (SELECT 
					PR.CODIGO,
					FB.Fantasia,
					YEAR(CB.DAT_ENTRADA) AS Ano_Entrada,
					MONTH(CB.DAT_ENTRADA) AS Mes_Entrada,
					 SUM(IT.Qtd_PedFat) qtd_comp
				FROM NFEIT IT
					JOIN NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
					JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
					JOIN FABRI FB ON PR.Cod_Fabricante = FB.CODIGO
				WHERE IT.Cod_Estabe = 1
				AND YEAR(CB.DAT_ENTRADA) > YEAR(GETDATE()) -1
				AND IT.Cod_Cfo in (2102, 2403, 2404)
				AND pr.Tipo = 00
				GROUP BY
					PR.CODIGO,
					FB.Fantasia,
					CB.DAT_ENTRADA,
					IT.Cod_Cfo ),

BONI AS (SELECT 
			PR.CODIGO,
			FB.Fantasia,
			YEAR(CB.DAT_ENTRADA) AS Ano_Entrada,
			MONTH(CB.DAT_ENTRADA) AS Mes_Entrada,
			SUM(IT.Qtd_PedFat) qtd_boni
		FROM NFEIT IT
			JOIN NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
			JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
			JOIN FABRI FB ON PR.Cod_Fabricante = FB.CODIGO
		WHERE IT.Cod_Estabe = 1
		AND YEAR(CB.DAT_ENTRADA) > YEAR(GETDATE()) -1
		AND pr.Tipo = 00
		AND IT.Cod_Cfo in (1910, 2910)
		GROUP BY
			PR.CODIGO,
			FB.Fantasia,
			CB.DAT_ENTRADA,
			IT.Cod_Cfo)
SELECT 
	DISTINCT
	Isnull(C.CODIGO, B.CODIGO) AS Codigo,
	Isnull(C.Fantasia,B.Fantasia) AS Fantasia,
	Isnull(C.Ano_Entrada, B.Ano_Entrada) AS Ano_Entrada,
	Isnull(C.Mes_Entrada, B.Mes_Entrada) AS Mes_Entrada,
	Isnull(C.qtd_comp, 0) as Qtd_Compra,
	IsNull(B.qtd_boni, 0) as Qtd_Bonif
	FROM COMPRA C
		FULL JOIN BONI B
	ON  C.Codigo = B.codigo
	AND C.ANO_ENTRADA = B.ANO_ENTRADA
	and C.Mes_Entrada = B.Mes_Entrada
	AND C.Codigo IS NOT NULL
GO


