SELECT 
	CB.Cod_EmiCliente AS 'CNPJ',
	CL.Razao_Social AS 'RAZAO SOCIAL',
	CL.Cod_Estado AS 'UF',
	CB.Numero AS 'NF DEVOLUÇÃO',
	SUBSTRING(CB.Str_RelDoc, 3, 10) AS 'NF VENDA',
	SCB.Dat_Emissao AS 'DATA DA NF DE VENDA',
	CASE 
		WHEN IT.Qtd_PedFat = SIT.Qtd_ImpFat THEN 'TOTAL'
		WHEN IT.Qtd_PedFat <> SIT.Qtd_ImpFat THEN 'PARCIAL'
	END 'TIPO DA DEVOLUCAO',
	pr.Cod_EAN AS 'EAN DO PRODUTO',
	pr.Descri AS 'DESCRICAO DO PRODUTO',
	it.Qtd_PedFat AS 'QUANTIDADE DEVOLVIDA',
	it.Vlr_BruItem AS 'VLR DA DEVOL P ITEM',
	cb.Obs_Rodape AS 'MOTIVO DEVOL',
	cb.Observacao AS 'OBSERVACOES'
FROM NFEIT IT
	JOIN NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
	JOIN ENXES ES ON CB.Cod_Estabe = ES.Cod_Estabe AND CB.Cod_EmiCliente = ES.Cod_Client
	JOIN CLIEN CL ON ES.Cod_Client = CL.Codigo
	left join NFSCB SCB ON CB.Cod_Estabe = SCB.Cod_Estabe AND SER_NOTA = 1 AND SUBSTRING(CB.Str_RelDoc, 3, 10) = NUM_NOTA AND CB.Cod_EmiCliente = SCB.Cod_Cliente
	JOIN NFSIT SIT ON SCB.Cod_Estabe = SIT.Cod_Estabe AND SCB.Ser_Nota = SIT.Ser_Nota AND SCB.Num_Nota = SIT.Num_Nota
					AND IT.Cod_Produto = SIT.Cod_Produto
	JOIN PRODU PR on it.Cod_Produto = pr.Codigo
WHERE
	pr.Cod_Fabricante = 319
and cb.Dat_Entrada >= '20260401'
and cb.Dat_Entrada <= '20260430'