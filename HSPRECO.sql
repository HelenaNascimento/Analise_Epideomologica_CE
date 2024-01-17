select HS.Cod_Produto, Dat_Alteracao, Vlr_PrcVen, Vlr_PrcFab, Vlr_PrcFab18, Vlr_PrcFab20
	from HSPRC HS
		INNER JOIN PRODU PR ON HS.Cod_Produto = PR.Codigo
WHERE HS.Cod_Estabe = 1
	AND PR.Cod_Fabricante = 123
	AND HS.Dat_Alteracao >= '20230101'
	AND HS.Dat_Alteracao <= '20231231'
ORDER BY 1, 2