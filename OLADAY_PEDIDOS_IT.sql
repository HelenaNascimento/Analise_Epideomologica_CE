

CREATE VIEW OLADAY_PEDIDOS_IT AS

SELECT  
	DISTINCT
	Cod_Produto,
	Qtd_Solicitado,
	Qtd_Pedido,
	Qtd_ImpFat,
	Vlr_Bruto,
	it.C_VlrDesconto,
	Per_Descon,
	IT.Id_PolCom as Id_Polcom_it,
	Numero,
	Dat_Pedido,
	it.Cod_MtvRej,
	it.Des_MtvRej
FROM PROD_2023.dbo.PDVCB CB
		JOIN PROD_2023.dbo.PDVIT IT ON CB.Cod_Estabe = IT.Cod_Estabe and CB.Numero = IT.Cod_Pedido
WHERE CB.COD_ESTABE = 1
	AND DAT_PEDIDO >= '20230101'
  

