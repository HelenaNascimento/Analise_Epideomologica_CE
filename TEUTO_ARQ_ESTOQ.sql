 SELECT      
	PRXES.Cod_Estabe,
	PRODU.Codigo AS Cidigo , 
	PRODU.Descricao AS Descricao, 
	PRODU.Cod_EAN AS Cod_EAN , 
	dbo.FN_EstoqueDisponivelPrd( PRXES.Cod_Estabe, PRODU.Codigo ) AS Qtd_Dispon, 
	PRXES.Qtd_Transi AS Qtd_Transi , 
	ACATU.Qtd_UniVen AS Qtd_VenMesAtu  	  , 
	ACANT.Qtd_UniVen AS Qtd_VenMesAnt  
FROM      PRXES AS PRXES      
	INNER JOIN PRODU AS PRODU ON (PRODU.Codigo = PRXES.Cod_Produt)  	  
	INNER JOIN FABRI AS FABRI ON (FABRI.Codigo = PRODU.Cod_Fabricante)  	  
	LEFT JOIN (SELECT Cod_Estabe AS Cod_Estabe, 
						ACPRD.Cod_Produto AS Cod_Produto, 
						ACPRD.Ano_Movimento AS Ano_Movimento, 
						ACPRD.Mes_Movimento AS Mes_Movimento, 
						ACPRD.Qtd_UniVen AS Qtd_UniVen FROM ACPRD AS ACPRD) AS ACATU ON ((ACATU.Cod_Estabe = PRXES.Cod_Estabe) AND (ACATU.Cod_Produto = PRODU.Codigo) 
						AND (ACATU.Ano_Movimento = 2024) AND (ACATU.Mes_Movimento = 9))  	  
	LEFT JOIN (SELECT Cod_Estabe AS Cod_Estabe, 
	ACPRD.Cod_Produto AS Cod_Produto,
	ACPRD.Ano_Movimento AS Ano_Movimento, 
	ACPRD.Mes_Movimento AS Mes_Movimento, 
	ACPRD.Qtd_UniVen AS Qtd_UniVen FROM ACPRD AS ACPRD) AS ACANT ON ((ACANT.Cod_Estabe = PRXES.Cod_Estabe) 
	AND (ACANT.Cod_Produto = PRODU.Codigo) AND (ACANT.Ano_Movimento = 2024) AND (ACANT.Mes_Movimento = 8))  
WHERE      ((PRXES.Cod_Estabe = 0) AND (dbo.FN_EAN13Ok(PRODU.Cod_EAN) = 1))  	
AND (EXISTS(SELECT LGXFB.Cod_Fabric AS Cod_Fabric FROM LGXFB AS LGXFB WHERE((LGXFB.Cod_Estabe = PRXES.Cod_Estabe) AND (LGXFB.Cod_Layout = 14) AND (LGXFB.Cod_Grupo = 1) 
AND (LGXFB.Cod_Fabric = FABRI.Codigo))))  
ORDER BY      PRODU.Cod_EAN ASC 