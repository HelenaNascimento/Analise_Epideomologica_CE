SELECT 
	IT.Id_PolCom,
	PC.Cod_PolCom,
	count(it.cod_produto) AS Qtd_Produtos,
	sum(C_VlrPedido) AS VlrPedido,
	sum(it.C_VlrDesconto) AS VlrDesconto,
	sum(C_VlrLiquido) AS VlrLiquido,
	CB.Dat_Pedido
FROM PDVIT IT
	JOIN PDVCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Cod_Pedido = CB.Numero
	LEFT JOIN POCOM PC ON IT.Id_PolCom = PC.Id_PolCom
WHERE IT.Cod_Estabe = 1
	AND CB.Dat_Pedido >= '20240925'
group by IT.Id_PolCom, PC.Cod_PolCom, CB.Dat_Pedido
ORDER BY 7