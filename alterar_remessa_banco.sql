select Cod_Barra, Num_Bloqueto, Dat_Remessa, Num_Remessa, Cod_ServRem from CTREC
where Cod_Estabe = 1 
	and Cod_Agente = 902 
	and Dat_Emissao >= '20240513'
	and Dat_Emissao <= '20240513'