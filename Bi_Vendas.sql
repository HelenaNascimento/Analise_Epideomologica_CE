USE DMD
GO

SELECT	TOP 5
		SUM(Vlr_TotalNota),
		CASE
			WHEN NFSCB.Cod_Estabe = 0 THEN 'NOVA PE'
			WHEN NFSCB.Cod_Estabe = 1 THEN 'NOVA CE'
			WHEN NFSCB.Cod_Estabe = 3 THEN 'MULTI'
			WHEN NFSCB.Cod_Estabe = 4 THEN 'NOVA BA'
			END AS 'Estabelecimento',
		NFSCB.Cod_Vendedor,
		NFSCB.Cod_VendTlmkt,
		FABRI.Fantasia
	FROM NFSCB
	INNER JOIN NFSIT
	ON NFSCB.Num_Nota = NFSIT.Num_Nota
	INNER JOIN PRODU
	ON NFSIT.Cod_Produto = PRODU.Codigo
	INNER JOIN FABRI
	ON PRODU.Cod_Fabricante = FABRI.Codigo
	INNER JOIN NFECB
	ON NFSCB.NUM_NOTA <> CAST(substring(NFECB.Str_RelDoc, 5, 8)  AS INT)
	WHERE YEAR(NFSCB.Dat_Emissao) = year(GETDATE() -1)
	AND   MONTH(NFSCB.Dat_Emissao) = MONTH(GETDATE() -1)
	AND   DAY(NFSCB.Dat_Emissao) = DAY(GETDATE() -1)
	AND   NFSCB.Status = 'F'
	AND   Tip_Saida = 'V'
	AND SUBSTRING(NFECB.Str_RelDoc, 1,1) = 1
	--AND   NFSCB.Num_Nota NOT IN(SELECT * FROM NFECB)
	--AND NOT EXISTS (SELECT Str_RelDoc FROM NFECB WHERE Str_RelDoc <> '' OR Str_RelDoc > 0  )
	GROUP BY NFSCB.Cod_Estabe, NFSCB.Cod_Vendedor, NFSCB.Cod_VendTlmkt, FABRI.Fantasia, NFSCB.Num_Nota
	ORDER BY NFSCB.Num_Nota