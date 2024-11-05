

CREATE VIEW OLADAY_POLCOM AS
SELECT 
	IT.Id_PolCom,
	PC.Cod_PolCom,
	count(it.cod_produto) AS Qtd_Produtos,
	sum(C_VlrPedido) AS VlrPedido,
	sum(it.C_VlrDesconto) AS VlrDesconto,
	sum(C_VlrLiquido) AS VlrLiquido,
	CB.Dat_Pedido
FROM PROD_2023.DBO.PDVIT IT
	JOIN PROD_2023.DBO.PDVCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Cod_Pedido = CB.Numero
	LEFT JOIN PROD_2023.DBO.POCOM PC ON IT.Id_PolCom = PC.Id_PolCom
WHERE IT.Cod_Estabe = 1
	AND CB.Dat_Pedido >= '20240101'
group by IT.Id_PolCom, PC.Cod_PolCom, CB.Dat_Pedido

