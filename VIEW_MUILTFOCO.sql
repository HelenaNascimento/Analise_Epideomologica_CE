SELECT * FROM FABRI
WHERE Fantasia LIKE '%MCG%'

-- MCG: 322, 1167, 1295
-- AGPMED: 772
-- KATIGUA 1423

SELECT 	
	IT.Cod_Cfo,
	--'saida codigo',
	cb.Dat_Emissao,
	it.Num_Nota,
	it.COD_ESTABE,
	it.Qtd_ImpFat,
	it.Vlr_BruItem,
	pr.Cod_EAN,
	cb.Cod_Vendedor,
	'nome vendedor',
	'vendedor ativo',
	cb.Cod_Cliente


FROM NFSIT IT
	JOIN PRODU PR ON IT.Cod_Promocao = PR.Codigo
	JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe and it.Ser_Nota = cb.Ser_Nota and it.Num_Nota = cb.Num_Nota
WHERE  PR.Cod_Fabricante IN (322,1167,1295,772,1423)