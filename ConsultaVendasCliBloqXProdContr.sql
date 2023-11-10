select 
		cl.codigo, 
		cl.val_cerreg, 
		cb.numero, 
		cb.dat_pedido, 
		pr.codigo, 
		pr.descri, 
		pr.Ctrl_venda,
		it.qtd_pra,
		cb.Cod_OrigemPdv
	from CLIEN cl
		inner join enxes es on cl.codigo= es.cod_client
		inner join pdvcb cb on cl.codigo = cb.cod_cliente and es.cod_estabe= cb.cod_estabe
		inner join pdvit it on cb.Cod_Estabe = it.cod_estabe and cb.numero = it.cod_pedido 
		left outer join produ pr on it.cod_produto = pr.codigo
		--inner join vende vd on cb.cod_vendedor = vd.codigo
where es.cod_estabe = 1 
	and cl.val_cerreg < = getdate()
	and cb.dat_pedido >= '20230705'
	--and cl.codigo = 12193
	and pr.Ctrl_venda = 'C'
	and it.qtd_pra >0
--	and cb.numero = 2184495
		

--select it.cod_produto, pr.*
--	from pdvit it
--	 inner join produ pr on it.cod_produto = pr.codigo  
--where cod_estabe = 1 
--	and cod_pedido = 2186516
--	and pr.descri like 'FENITOINA%'	
--	and ctrl_venda = 'C'