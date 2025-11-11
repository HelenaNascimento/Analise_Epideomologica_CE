

WITH POLITICA AS 
	(SELECT 
		PR.Id_PolCom,
		PR.Cod_Produt,
		vlr_ven = (PES.Prc_Fabric - (PES.Prc_Fabric * (PR.Per_Descon / 100)))
	FROM POCOM PC
		JOIN PCXES ES ON PC.Id_PolCom = ES.Id_PolCom
		JOIN PCXPR PR ON PC.Id_PolCom = PR.Id_PolCom
		JOIN PRXES PES ON ES.Cod_Estabe = PES.Cod_Estabe AND PR.Cod_Produt = PES.Cod_Produt
	WHERE ES.Cod_Estabe = 1
	AND PC.Dat_Termino >= GETDATE()
	AND PC.Bloqueado = 0) ,
PEDIDO AS (SELECT 
				IT.Cod_Produto,
				IT.Id_PolCom,
				CB.Numero,
				it.Vlr_Bruto,
				it.Per_Descon,
				it.Prc_Unitario,
				IT.Qtd_Pedido,
				IT.Prc_UniLiqPer
				FROM PDVCB CB 
					JOIN PDVIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Numero = IT.Cod_Pedido
				WHERE CB.Cod_Estabe = 1
				AND year(CB.Dat_Pedido) = year(getdate())
				and month(CB.Dat_Pedido) = month(getdate())
				and day(CB.Dat_Pedido) = day(getdate())
				AND CB.Cod_OrigemPdv = 'AL')
SELECT 
		PE.Cod_Produto AS COD_PRODUTO, 
		PE.Numero AS NUM_PEDIDO,
		PL.Id_PolCom AS ID_POLITICA,
		format(PE.Prc_Unitario, 'c', 'pt-br') AS PRC_UNIT,
		PE.QTD_PEDIDO,
		CONVERT (decimal(10,2), PE.PER_DESCON) AS PER_DESCONT,
		format(PE.Vlr_Bruto, 'c', 'pt-br') AS VLR_BRUTO,		
		pRCxqTD = Format((PE.Prc_Unitario * PE.Qtd_Pedido), 'c', 'pt-br'),
		format(PL.vlr_ven, 'c', 'pt-br') as VLR_CALC,
		format(PE.Prc_UniLiqPer, 'c', 'pt-br') AS VLR_PED
FROM POLITICA PL, PEDIDO PE
WHERE PE.Cod_Produto = PL.Cod_Produt
AND PE.Id_PolCom = PL.Id_PolCom
AND  PL.vlr_ven <> PE.Prc_UniLiqPer