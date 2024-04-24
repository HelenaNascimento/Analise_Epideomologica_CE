SELECT DISTINCT
	CB.Numero AS 'orderExternalId',
    CB.Numero AS 'transientOrderExternalId', 
    CB.Cod_PedCli AS 'orderNexfarId',
    CB.Cod_Cliente AS 'clientId',
    CL.Cgc_Cpf AS 'cnpj', 
    CB.Cod_Vendedor AS 'sellerId', 
    CASE 
		WHEN CB.Status1 = 'P' THEN 'RECEIVED_BY_DISTRIBUTOR' 
		WHEN CB.Status1 = 'C' THEN 'CANCELED' 
		WHEN CB.Status1 = 'D' and NF.Ret_CStat = 100 and NF.Dat_Saida is null THEN 'BILLED' 
		WHEN CB.Status1 = 'D' and NF.Ret_CStat = 100 and NF.Dat_Saida IS NOT NULL THEN 'IN_TRANSIT'
    END AS 'status', 
    CB.Dat_Pedido AS 'orderDate'
    FROM DMD.dbo.PDVCB CB
        INNER JOIN DMD.dbo.CLIEN CL ON CB.Cod_Cliente = CL.Codigo
		LEFT JOIN DMD.dbo.NFSCB NF ON  CL.CODIGO = NF.Cod_Cliente 
						AND CB.COD_ESTABE = NF.COD_ESTABE 
						AND CB.Cod_NumNfsIni = NF.Num_Nota 
						AND nf.Ser_Nota = '1'
    WHERE CB.Cod_PedCli like '129524926819688';


--select top 10 * fROM DMD.dbo.PDVCB CB