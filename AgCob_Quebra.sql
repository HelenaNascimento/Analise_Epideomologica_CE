SELECT 
	Cod_Documento,
	Num_Documento,
	Dat_Emissao,
	Dat_Remessa,
	Num_Remessa,
	Num_Bloqueto

FROM CTREC
WHERE COD_ESTABE = 1 AND Cod_Agente = 344 --AND Num_Bloqueto in (393, 394, 395, 396, 397)
order by 5