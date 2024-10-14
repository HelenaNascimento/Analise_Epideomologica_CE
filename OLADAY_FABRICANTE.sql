

CREATE VIEW OLADAY_FABRICANTE AS

SELECT  
	C_VlrDesconto,
	C_VlrLiquido,
	Cod_Produto,
	Dat_Pedido,
	Id_Polcom_it,
	Numero,
	Per_Descon,
	Qtd_ImpFat
FROM PROD_2023.dbo.FABRI FB
		JOIN PROD_2023.dbo.FBXES ES ON FB.Codigo = ES.Cod_Fabric
WHERE ES.COD_ESTABE = 1

  

