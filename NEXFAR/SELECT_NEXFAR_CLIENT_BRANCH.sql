WITH PaymentConditions AS (
		SELECT 
			PCXPZ.Id_PolCom, 
			STRING_AGG(CONVERT(VARCHAR, PRZCB.Cod_Prz), '/') AS Payment
		FROM PCXPZ 
		JOIN PRZCB ON PCXPZ.Cod_TabPrz = PRZCB.Cod_Prz
		WHERE PCXPZ.Id_PolCom IN (3005, 3015, 3004, 3003)
		GROUP BY PCXPZ.Id_PolCom
	),
	CLIENTE AS (SELECT [CODIGO], [ID_POLCOM], [COD_ESTADO], [COD_ESTABE]
					FROM  DMD.dbo.VIEW_NEXFAR_CLIENTE)
	SELECT DISTINCT 
		CLIEN.Codigo AS 'clientId',
		ENXES.Cod_Estabe AS 'branchId',
		ENXES.Cod_Estabe AS 'warehouseId',
		POCOM.Id_PolCom AS 'priceGroupId',
		ENXES.Cod_Vendedor AS 'sellerId',
		'0' AS 'mainSeller',
		'NEXON' AS 'module',
		CLIEN.Cgc_Cpf AS 'cnpj',
 
		PC.Payment AS 'paymentConditions',

		POCOM.Vlr_Minimo AS 'orderMinValue',
    
		CASE 
			WHEN CLIEN.Inscricao_Municipal = 'ISENTO' THEN 1
			ELSE 0
		END AS 'taxFree',

		Cod_RegTri AS 'clientTaxCode',
		CONVERT(VARCHAR, ENXES.Cod_Transp) AS 'ep_CodTransportadora',
		CONVERT(VARCHAR, ENXES.Cod_Rota) AS 'ep_CodRota',
		CONVERT(VARCHAR, ENXES.Cod_AgeCob) AS 'ep_CodAgente'
    
	FROM 
		CLIENTE
		JOIN ENXES ON CLIENTE.COD_ESTABE = ENXES.Cod_Estabe
		JOIN CLIEN ON ENXES.Cod_Client = CLIEN.Codigo
		JOIN VENDE ON VENDE.Codigo = ENXES.Cod_Vendedor		
		JOIN POCOM ON CLIENTE.ID_POLCOM = POCOM.Id_PolCom
		LEFT JOIN PaymentConditions AS PC ON PC.Id_PolCom = POCOM.Id_PolCom