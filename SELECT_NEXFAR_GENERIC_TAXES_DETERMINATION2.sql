SELECT DISTINCT
	 PRXES.Cod_Estabe AS 'warehouseId'
	,CASE
		WHEN PRXES.Cod_Estabe = 0 THEN 'PE'
		WHEN PRXES.Cod_Estabe = 1 THEN 'CE'
		WHEN PRXES.Cod_Estabe = 3 THEN 'PE'
		WHEN PRXES.Cod_Estabe = 4 THEN 'BA'
		END AS 'warehouseUf'
	,PRXES.Cod_Produt AS 'sku'
	,CASE
		WHEN PRXES.Cod_Estabe = 0 THEN 'PE'
		WHEN PRXES.Cod_Estabe = 1 THEN 'CE'
		WHEN PRXES.Cod_Estabe = 3 THEN 'PE'
		WHEN PRXES.Cod_Estabe = 4 THEN 'BA'
		END AS 'clientTaxCode'
	,PRXES.Cod_ClaTri AS 'productTaxCode'
	FROM PRXES
	INNER JOIN PCXPR
		ON PCXPR.Cod_Produt = PCXPR.Cod_Produt
	INNER JOIN POCOM
		ON POCOM.Id_PolCom = PCXPR.Id_PolCom
	WHERE POCOM.Flg_Web = 1
	AND   POCOM.Dat_Termino > GETDATE()
	AND   PRXES.Cod_ClaTri IS NOT NULL
	ORDER BY PRXES.Cod_Produt ASC