
SELECT 	
	DISTINCT
	CNPJ_Filial_Distribuidor = 
	case
		when it.COD_ESTABE = 0 then '63400543000388'
		when it.COD_ESTABE = 1 then '63400543000116'
		when it.COD_ESTABE = 3 then '28934740000114'
		when it.COD_ESTABE = 4 then '63400543000469'
		else ''
	end,
	'CNPJ_Industria',
	cb.Dat_Emissao AS Data_Nota_Fiscal,
	it.Num_Nota AS Numero_Nota_Fiscal,
	cb.chv_acesso AS Chave_NFE,
	pr.Cod_EAN AS EAN,
	it.Cod_Produto AS Cod_interno,
	PR.DESCRI AS Nome_Produto,
	PR.Unidade_Venda AS Unidade_Venda,
	Fat_CnvImpFat AS Fator_Para_Pacote, --
	CASE
		WHEN Tip_Saida = 'V' THEN 'F'
		ELSE Tip_Saida
	END
	AS Tipo_Documento,
	'R' AS Tipo_Envio,
	IT.Vlr_BruItem AS VendaValorBruta,
	IT.Vlr_LiqItem AS VendaValorLiquida,
	(IT.Qtd_Bonificacao + it.Qtd_ImpFat) AS VendaUnidades,
	IT.Prc_UniImpFat AS Preco_Sku_NF,
	CASE 
		WHEN cb.Cod_OrigemNfs = 'ML' THEN 1
		WHEN cb.Cod_OrigemNfs = 'AL' THEN 2
		WHEN cb.Cod_OrigemNfs = 'AL' THEN 3 -- Ecommerce Distribuido
		WHEN cb.Cod_OrigemNfs = 'TL' THEN 7

	end as 'Canal_Venda',
	cb.Cod_Vendedor AS Cod_Vendedor_Hierarquia,
	'Cod_Fiscal_Vendedor',
	cl.Cgc_Cpf AS  CNPJ_PDV,
	1 AS 'Formato_PDV',
	CL.Razao_Social,
	CL.Cod_Estado AS UF,
	CD.Descricao,
	cl.Cep,
	cl.Endereco,
	cl.Numero,
	PDV.Numero AS COD_PEDIDO,
	PDV.Dat_Pedido
FROM NFSIT IT
	JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe and it.Ser_Nota = cb.Ser_Nota and it.Num_Nota = cb.Num_Nota
	JOIN CLIEN CL ON cb.Cod_Cliente = cl.Codigo
	JOIN CIDAD CD ON CL.Cod_Cidade = CD.Codigo
	JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
	JOIN VENDE VE ON CB.Cod_Vendedor = VE.Codigo
	JOIN PDVCB PDV ON CB.Cod_Estabe = PDV.Cod_Estabe AND CB.Cod_Pedido = PDV.Numero
WHERE IT.Cod_Estabe = 4
AND Dat_Emissao >= DATEADD(MONTH, -1, GETDATE())  
AND Pr.Cod_Fabricante IN (338)



