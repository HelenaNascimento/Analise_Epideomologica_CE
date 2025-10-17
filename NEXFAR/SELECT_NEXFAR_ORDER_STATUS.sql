SELECT DISTINCT 
	pdv.Numero AS 'orderExternalId',
	pde.Cod_PedCli AS 'orderNexfarId',
	pde.Num_PedVen AS 'transientOrderExternalId',
	pde.Cod_Client AS 'clientId',
	pde.Num_Cnpj AS 'cnpj',
	pdv.Cod_Vendedor AS 'sellerId',
	CASE
		WHEN pdv.Status1 = 'P' and pdv.Status2 = 'F'  THEN 'RECEIVED_BY_DISTRIBUTOR'
		WHEN (pdv.Status1 = 'C' OR (pde.Num_PedVen = 0 AND pdv.Numero IS NULL)) THEN 'CANCELED'
		WHEN pdv.Status1 = 'P' and pdv.Status2 = 'N' THEN 'BILLED'
		WHEN pdv.Status1 = 'D'AND NF.Dat_Saida IS NOT NULL AND Dat_RetEnt IS NULL  THEN 'IN_TRANSIT'
		WHEN pdv.Status1 = 'D' AND pdv.Status2 <> 'A'  THEN 'DELIVERED'
		END                      AS 'status',
	CAST(pde.Dat_LeiPed AS DATE) AS 'orderDate',
	CAST(pdv.Hor_Fatura AS DATE) AS 'billingDate'
FROM PDECB pde
            LEFT JOIN PDVCB pdv ON pde.Cod_Estabe = pdv.Cod_Estabe AND pde.Cod_PedCli = pdv.Cod_PedCli
            INNER JOIN PDEIT pdit on pde.Cod_Estabe = pdit.Cod_Estabe and pde.Cod_PedCli = pdit.Cod_PedCli
            LEFT OUTER JOIN PDVIT pit on pdv.Cod_Estabe = pit.Cod_Estabe and pdv.Numero = pit.Cod_Pedido and
                                        pdit.Cod_Produt = pit.Cod_Produto
            LEFT JOIN NFSCB NF ON pde.Cod_Client = NF.Cod_Cliente
    AND pdv.COD_ESTABE = NF.COD_ESTABE
    AND pdv.Cod_NumNfsIni = NF.Num_Nota
--WHERE pdv.NUMERO = 51085382  
GROUP BY pdv.Numero,
            pde.Cod_PedCli,
            pde.Num_PedVen,
            pde.Cod_Client,
            pde.Num_Cnpj,
            pdv.Cod_Vendedor,
            pdv.Status1,
			pdv.Status2,
            NF.Ret_CStat,
            NF.Dat_Saida,
            Dat_RetEnt,
            pde.Dat_LeiPed,
            pdv.Hor_Fatura;
