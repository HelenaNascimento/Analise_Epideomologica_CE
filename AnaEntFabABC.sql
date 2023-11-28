
create view AnaEntFabABC as

select 
	distinct
	pr.Cod_Fabricante,
	cod_ean,
	codigo,
	descricao,
	CB.dat_entrada,
	format(Prc_UniFat, 'c', 'pt-br') as Prc_UniFat,
	SUM(it.Qtd_PedFat) AS Qtd_Comp,
	it.protocolo,
	format(ES.prc_venda, 'c', 'pt-br') as Prc_Venda_Atual,
	Custo = case 
			when es.Prc_CusMed > 0 then format(es.Prc_CusMed, 'c', 'pt-br') 
			when es.Prc_CusMed = 0 then format(Prc_CusLiqEnt, 'c', 'pt-br')
	end
	from PRODU PR
		Inner join PRXES ES ON PR.CODIGO = ES.COD_PRODUT
		inner join NFEIT IT ON PR.CODIGO = IT.Cod_Produto AND ES.COD_ESTABE = IT.COD_ESTABE 
		inner join NFECB CB ON IT.COD_ESTABE = CB.COD_ESTABE  AND IT.PROTOCOLO = CB.PROTOCOLO
	 WHERE ES.Cod_Estabe = 1
	AND CB.STATUS IN NOT ('A', 'C')
	AND pr.Cod_Fabricante in (158, 319, 123, 321, 588, 338, 33, 237, 164, 1022)
	AND cb.Dat_Entrada >= '20210101'
	AND cb.Dat_Entrada <= '20231031'

	group by
		pr.Cod_Fabricante,
		cod_ean,
		codigo,
		descricao,
		CB.dat_entrada,
		it.protocolo,
		Prc_UniFat, 
		ES.prc_venda,
		es.Prc_CusMed,
		Prc_CusLiqEnt

