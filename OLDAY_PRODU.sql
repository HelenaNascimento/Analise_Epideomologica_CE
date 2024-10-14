SELECT 
	distinct
	PR.codigo,
	PR.Descricao,
	--FB.Fantasia,
	--IT.Id_PolCom,
	--PC.Cod_PolCom,
	sum(it.Qtd_Pedido) AS Qtd_Produtos,
	--AVG (IT.Prc_LiqUltEnt) MED_Prc_LiqUltEnt,
	--AVG(IT.Prc_Unitario) MED_Prc_Unitario,
	sum(it.Vlr_Bruto) AS VlrPedido,
	sum(it.Per_Desconto) AS VlrDesconto,
	--IT.Cod_Pedido,
	CB.Dat_Pedido
FROM PDVIT IT
	JOIN PDVCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Cod_Pedido = CB.Numero
	LEFT JOIN POCOM PC ON IT.Id_PolCom = PC.Id_PolCom
	LEFT JOIN PRODU PR ON IT.cod_produto = PR.Codigo
	LEFT JOIN FABRI FB ON PR.Cod_Fabricante = FB.Codigo
WHERE IT.Cod_Estabe = 1
	AND CB.Dat_Pedido >= '20240926'
	AND CB.Dat_Pedido <= '20240926'
group by 
	PR.codigo,
	PR.Descricao,
	--FB.Fantasia,
	--IT.Id_PolCom,
    --PC.Cod_PolCom,
	CB.Dat_Pedido
ORDER BY 1 --, 10