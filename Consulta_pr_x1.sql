
SELECT distinct   
		COD_PRODUT,
		CONVERT(DECIMAL(10,2),PRC_ULTENT) as ultEntr,
		CONVERT(DECIMAL(10,2), ent.Prc_Unitario) as 'ultEnt > 0',
		ent.Dat_Entrada
	FROM PRXES es
		INNER JOIN (SELECT distinct
					it.Cod_Produto as Cod_Produto
					, MAX(IT.Protocolo) as Protocolo
					, cb.Cod_Estabe as estabe
					, it.Prc_Unitario
					, MAX(CB.Dat_Entrada) as Dat_Entrada
					FROM NFEIT IT 
						INNER JOIN NFECB CB ON IT.PROTOCOLO = CB.PROTOCOLO and it.Cod_Estabe = cb.Cod_Estabe
					where convert(decimal(10,2), Prc_Unitario) > 0.00
					group by Cod_Produto, 
							cb.Cod_Estabe,
							it.Prc_Unitario) Ent on es.Cod_Produt = ent.Cod_Produto and es.Cod_Estabe = ent.estabe					 
WHERE COD_ESTABE = 1 
AND CONVERT(DECIMAL(10,2), PRC_ULTENT) = 0.00
AND QTD_FISICO > 0

order by COD_PRODUT, Dat_Entrada


select PROTOCOLO, * from NFEIT
WHERE COD_ESTABE = 1
	AND COD_PRODUTO = 6029