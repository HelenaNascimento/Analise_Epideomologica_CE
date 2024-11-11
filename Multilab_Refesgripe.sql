SELECT 
	Distinct
	CL.Razao_Social,
	PR.Descricao,
	convert(int, sum(it.Qtd_ImpFat)) as Faturado,
	convert(int, it.Qtd_ImpFat/4) as Bonificacao,	
	concat('.', cb.Chv_Acesso) as Chv_Acesso
	FROM NFSCB CB
		JOIN NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota and CB.Num_Nota = IT.Num_Nota
		left join PRODU PR ON IT.Cod_Produto = PR.Codigo
		left join CLIEN CL ON CB.Cod_Cliente = CL.Codigo
WHERE CB.Cod_Estabe = 1
AND PR.Codigo in (16442) --16395 / 16442
AND CB.Dat_Emissao >= '20241029'
AND CB.Dat_Emissao <= '20241031'
--AND it.Cod_Cfo = 5910
AND CB.Tip_Saida = 'V'
AND it.Qtd_ImpFat >= 4
group by 	CL.Razao_Social,
			PR.Descricao,
			cb.Chv_Acesso,
			it.Qtd_ImpFat