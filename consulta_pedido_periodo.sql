select distinct
	fb.Fantasia
	--cb.numero,
	--cb.id_polcom,
	--pc.Cod_PolCom,
	--Cod_Cliente,
	--cl.Razao_Social,
	--format(cb.Dat_Pedido, 'd', 'en-gb') as Dat_Pedido,
	--it.cod_produto,
	--pr.descri,
	--sum(it.C_VlrLiquido) as vlr_liq,
	--format(cb.Vlr_LiqItens, 'c', 'pt-br') as Vlr_LiqItens,
	--format(cb.C_VlrPedido, 'c', 'pt-br') as Vlr_Pedido,
	--CONVERT(VARCHAR(200), cb.observacao) AS observacao
	from PDVCB cb
		inner join CLIEN cl on cb.cod_cliente = cl.codigo
		inner join PDVIT it on cb.cod_estabe = it.cod_estabe and cb.Numero = it.Cod_Pedido
		inner join PRODU  pr on it.cod_produto = pr.codigo
		inner join FABRI fb on pr.cod_fabricante = fb.codigo
		inner join POCOM pc on cb.id_polcom = pc.id_polcom
where cb.cod_estabe = 1
	and CONVERT(VARCHAR(200), cb.observacao)  <> '     '
	and cb.Dat_Pedido >= '20230928'
		and cb.Dat_Pedido <= '20231001'
		--and cb.numero = 2302315
--group by
--	fb.Fantasia,
--	cb.numero,
--	cb.id_polcom,
--	pc.Cod_PolCom,
--	Cod_Cliente,
--	cl.Razao_Social,
--	cb.Dat_Pedido,
--	it.cod_produto,
--	pr.descri,
--	it.C_VlrLiquido,
--	cb.Vlr_LiqItens,
--	cb.C_VlrPedido,
--	CONVERT(VARCHAR(200), cb.observacao)
	
order by 1




create view vw_obspedido_ce as 
select 
	Cod_Cliente,
	cl.Razao_Social,
	format(cb.Dat_Pedido, 'd', 'en-gb') as Dat_Pedido,
	CONVERT(VARCHAR(200), cb.observacao) AS observacao
	from PDVCB cb
		inner join CLIEN cl on cb.cod_cliente = cl.codigo
where cb.cod_estabe = 1
	and CONVERT(VARCHAR(200), cb.observacao)  <> '     '
	and cb.Dat_Pedido >= '20230101'
		and cb.Dat_Pedido <= getdate()

