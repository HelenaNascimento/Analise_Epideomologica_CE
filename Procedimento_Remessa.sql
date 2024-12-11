SELECT 
concat(tip_documento, num_documento, par_documento) as Numero_Documento,
Cl.Razao_Social,
format(Dat_Emissao, 'd', 'pt-br') as Emissao,
format(Dat_Vencimento, 'd', 'pt-br' ) as Vencimento,
Dias_Atraso = case
	when cast(((getdate()-1) - Dat_Vencimento)as int) <= 0 then 0
	when cast(((getdate()-1) - Dat_Vencimento)as int) > 0 then  cast(((getdate()-1) - Dat_Vencimento)as int)
end
FROM CTREC CT
	LEFT OUTER JOIN CLIEN CL ON CT.cod_cliente = CL.codigo
WHERE Cod_Estabe = 0 
AND Cod_Agente = 341
AND Dat_Emissao = '20241008'
AND Dat_Remessa = '20241009'
AND Num_Documento < '1536762'
AND Status <> 'Q'
