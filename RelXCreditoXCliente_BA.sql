SELECT 
	CL.CODIGO as CODIGO_CLIENTE,
	CL.RAZAO_SOCIAL,
	CL.Cgc_Cpf as CNPJ,
	IsNull(Format(Limite_Credito, 'c', 'pt-br'), 0) as Limite_Credito,
	IsNull(Format(Total_Debito, 'c',  'pt-br'), 0) as Total_Debito,
	IsNull(Atraso_MedAtu, 0) as Atraso_MedAtu,
	IsNull(Prz.Qtd_Med_Praz,0) as Qtd_Med_Praz,
	format(sum(ct.Vlr_Documento), 'c', 'pt-br') AS 'Valor_Documento'
FROM CLIEN CL
	JOIN ENXES ES ON CL.CODIGO = ES.Cod_Client 
	JOIN CTREC CT ON ES.Cod_Estabe = CT.Cod_Estabe AND CL.Codigo = CT.Cod_Cliente
	JOIN (SELECT distinct cod_estabe, cod_cliente, CONVERT(INT, (SUM(Qtd_PrzMed) / COUNT(Qtd_PrzMed))) as Qtd_Med_Praz FROM NFSCB WHERE Cod_Estabe = 4 group by cod_estabe, cod_cliente) PRZ 
		on es.cod_estabe = prz.Cod_Estabe and cl.codigo = PRZ.cod_cliente
WHERE
	ES.Cod_Estabe = 4
AND CL.bloqueado = 0
AND CT.Dat_Vencimento <= GETDATE()
GROUP BY
	CL.CODIGO,
	CL.RAZAO_SOCIAL,
	CL.Cgc_Cpf,
	Limite_Credito,
	Total_Debito,
	Atraso_MedAtu,
	PRZ.Qtd_Med_Praz
order by 1