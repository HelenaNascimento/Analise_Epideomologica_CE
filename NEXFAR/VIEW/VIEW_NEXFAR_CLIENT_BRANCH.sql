USE DMD
GO

CREATE VIEW VIEW_NEXFAR_CLIENT_BRANCH AS

SELECT DISTINCT CLIEN.Codigo AS 'clientId'
	,PCXES.Cod_Estabe AS 'branchID'
	,PCXES.Cod_Estabe AS 'warehouseId'
	,POCOM.Id_PolCom AS 'priceGroupId'
	,PCXVE.Cod_Vended AS 'sellerId'
	,'0' AS 'mainSeller'
	,'NEXON' AS 'module'
	,CLIEN.Cgc_Cpf AS 'cnpj'
	,'30, 30/60, 40/60/75, 60, 7, 45/60/75' AS 'paymentConditions'
	,POCOM.Vlr_Minimo AS 'orderMinValue'
FROM CLIEN
INNER JOIN PCXCL ON PCXCL.Cod_Client = CLIEN.Codigo
INNER JOIN POCOM ON PCXCL.Id_PolCom = POCOM.Id_PolCom
INNER JOIN PCXES ON PCXCL.Id_PolCom = PCXES.Id_PolCom
INNER JOIN PCXPZ ON PCXCL.Id_PolCom = PCXPZ.Id_PolCom
INNER JOIN PRZCB ON PCXPZ.Cod_TabPrz = PRZCB.Cod_Prz
INNER JOIN PCXVE ON PCXPZ.Id_PolCom = PCXVE.Id_PolCom
WHERE POCOM.Flg_Web = 1
	AND POCOM.Dat_Termino > GETDATE()
	ORDER BY clientId ASC






