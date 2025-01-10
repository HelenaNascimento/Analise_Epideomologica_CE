--select Cod_VendTlmkt, Cod_Vendedor, Dat_Pedido, COUNT(NUMERO) from PDVCB
--WHERE COD_ESTABE = 1 
--	AND Dat_Pedido >='20250102' 
--	AND Status1 = 'C'
--	AND Cod_OrigemPdv = 'ML'
--GROUP BY Cod_VendTlmkt, Cod_Vendedor, Dat_Pedido
--order by 3


SELECT 
CB.Cod_Cliente,
Or_Pedido = case
			when Cod_OrigemPdv = 'AL' then 'Ativo'
			when Cod_OrigemPdv = 'ML' then 'Móvel'
			when Cod_OrigemPdv = 'TL' then 'OL'
			end,
cb.Dat_Pedido,
COUNT(NUMERO) AS QTD_PEDIDO,
VE.Nome_Guerra,
V1.Nome_Guerra,
format(CB.C_VlrPedido, 'c', 'pt-br') as Vlr_Pedido
FROM PDVCB  CB
	JOIN VENDE VE on CB.Cod_Vendedor = VE.codigo
	JOIN VENDE V1 ON CB.Cod_VendTlmkt = V1.Codigo
	JOIN (SELECT 
				Dat_Pedido, 
				Cod_Cliente,
				C_VlrPedido
				FROM PDVCB Where COD_ESTABE = 1  
					AND Dat_Pedido >='20250102' 
					AND  Cod_OrigemPdv = 'AL' 
					AND Status1 <> 'C'
					) pass on CB.Dat_Pedido = pass.Dat_Pedido and CB.Cod_Cliente = pass.Cod_Cliente AND CB.C_VlrPedido = PASS.C_VlrPedido
WHERE COD_ESTABE = 1 
	AND CB.Dat_Pedido >='20250102' 
	AND CB.Status1 = 'C'
	AND Cod_OrigemPdv = 'ML'
GROUP BY cb.Cod_Cliente, Cod_OrigemPdv, VE.Nome_Guerra, cb.Dat_Pedido, V1.Nome_Guerra, Cod_OrigemPdv, CB.C_VlrPedido
order by 6 
