
select 
	--cb.Numero,
	FORMAT(sum(it.Prc_Unitario), 'c', 'pt-br') as vlr_politica,
	sum(Per_descon)
	from PDVCB CB
		inner join PDVIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.NUMERO = IT.COD_PEDIDO
WHERE CB.COD_ESTABE = 1 AND
	CB.Cod_Digitador = 591 AND
	TIP_FATURAMENTO = 'FAT' AND
	--CB.STATUS = 'D' AND
	CB.DAT_PEDIDO >= '20231001' AND
	CB.DAT_PEDIDO <= '20231030' AND
	IT.ID_POLCOM = 2854
--GROUP BY cb.Numero
	

SELECT TOP 5 * FROM PDVIT


select  it.PRC_UNITARIO, IT.PRC_UNIFAT, IT.* 
from NFECB cb
	inner join NFEIT it on cb.cod_estabe = it.cod_estabe and cb.protocolo = it.protocolo 
where cb.cod_estabe = 1  
	and Cod_EmiFornec = 288
	and status = 'F'
	and cb.protocolo = 130145


SELECT PRC_UNITARIO, COD_PRODUTO,  IT.* 
	FROM PDCCB cb
		inner join PDCIT it on cb. cod_estabe = it.cod_estabe and cb.numero = it.numero
WHERE CB.COD_ESTABE = 1 
	AND Cod_Fornec = 288
	AND COD_PRODUTO IN (35, 10820)
	AND CB.DAT_EMISSAO  >= '20230901'
ORDER BY 2
