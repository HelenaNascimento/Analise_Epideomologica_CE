SELECT 
	DISTINCT
	pr.Codigo,
	pr.Descri,
	dpr.Cod_LocFis
FROM
	DPXPR DPR
		INNER JOIN PRODU PR ON DPR.Cod_Produt = PR.Codigo
where dpr.Cod_Estabe = 1
	and dpr.Num_Rua BETWEEN 1 and 34
	and dpr.Qtd_Dispon > 0
	and pr.Flag_ImprClassif1 <> 'N'
ORDER BY 1
