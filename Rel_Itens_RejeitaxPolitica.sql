SELECT
	DISTINCT
	Estabelecimento = 
		case 
			when cb.Cod_Estabe = 0 then 'NOVA -PE'
			when cb.Cod_Estabe = 3 then 'NOVA MULT'
		end,
	CB.Cod_Vendedor,
	VD.Nome_Guerra,
	IT.COD_PRODUTO, 
	IT.Qtd_Solicitado, 
	IT.Des_MtvRej, 
	Cod_Pedido,
	cb.Cod_Cliente
	FROM PDVIT IT
		JOIN PDVCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Cod_Pedido = CB.Numero 
		left join VENDE VD ON CB.Cod_Vendedor = VD.Codigo
WHERE IT.Cod_Estabe IN (0,3)
AND CB.Cod_OrigemPdv = 'ML'
AND IT.Des_MtvRej LIKE 'Política%'
AND CB.Dat_Pedido >= '20250122'
ORDER BY 1, 7