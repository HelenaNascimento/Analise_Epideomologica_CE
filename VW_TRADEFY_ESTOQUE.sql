
SELECT 	
	DISTINCT
	CNPJ_Filial_Distribuidor = 
	case
		when ES.COD_ESTABE = 0 then '63400543000388'
		when ES.COD_ESTABE = 1 then '63400543000116'
		when ES.COD_ESTABE = 3 then '28934740000114'
		when ES.COD_ESTABE = 4 then '63400543000469'
		else ''
	end,
	'CNPJ_Industria',
	getdate() as Data_Posicao, --
	pr.Cod_EAN AS EAN,
	pr.Cod_EanEmbCmp AS DUN,
	pr.codigo as Cod_interno,
	LT.Cod_Lote,
	LT.Dat_Vencim AS Data_Validade,
	PR.DESCRI AS Nome_Produto,
	PR.Unidade_Venda AS Unidade_Venda,
	'1' AS Fator_Para_Pacote, -- PR.Qtd_FraVen

FROM PRODU PR 
	JOIN PRXES ES ON PR.CODIGO = ES.COD_PRODUT
	JOIN PRLOT LT ON ES.Cod_Estabe = LT.Cod_Estabe AND ES.Cod_Produt = LT.Cod_Produt
WHERE ES.Cod_Estabe = 4
AND  Pr.Cod_Fabricante IN (338)



