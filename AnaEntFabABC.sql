

/*
select * 
	from AnaEntFabABC
where Cod_Fabricante = 123
order by 3
*/


--drop view AnaEntFabABC

create view vw_AnaEntFabABC as
select 
	distinct
	pr.Cod_Fabricante,
	FB.Fantasia,
	cod_ean,
	PR.Codigo,
	descricao,
	CB.dat_entrada,
	Prc_UniFat,
	SUM(it.Qtd_PedFat) AS Qtd_Comp,
	it.protocolo,
	ES.prc_venda,
	it.cod_lote,
	Custo = case 
			when es.Prc_CusMed > 0 then es.Prc_CusMed 
			when es.Prc_CusMed = 0 then Prc_CusLiqEnt
	end
	from PRODU PR
		Inner join PRXES ES ON PR.CODIGO = ES.COD_PRODUT
		inner join NFEIT IT ON PR.CODIGO = IT.Cod_Produto AND ES.COD_ESTABE = IT.COD_ESTABE 
		inner join NFECB CB ON IT.COD_ESTABE = CB.COD_ESTABE  AND IT.PROTOCOLO = CB.PROTOCOLO
		left join FABRI FB  ON PR.Cod_Fabricante = FB.Codigo
	 WHERE ES.Cod_Estabe = 1
	AND CB.status NOT IN ('A', 'C')
	AND pr.Cod_Fabricante in (158,319,123,321,588,338,33,237,164,1022)
	AND cb.Dat_Entrada >= '20230101'
	AND cb.Dat_Entrada <= '20231031'

	group by
		pr.Cod_Fabricante,
		FB.Fantasia,
		cod_ean,
		PR.Codigo,
		descricao,
		CB.dat_entrada,
		it.protocolo,
		Prc_UniFat, 
		ES.prc_venda,
		it.cod_lote,
		es.Prc_CusMed,
		Prc_CusLiqEnt