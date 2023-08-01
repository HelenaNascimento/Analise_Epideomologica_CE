select distinct
    DATENAME (MONTH, dat_emissao)  as Mes,
    sum(ct.Vlr_Documento),
    sum(ct.Vlr_DescConced)
	from CTREC ct
where ct.cod_estabe = 1
    and dat_emissao >= '20230101' 
    and Dat_Emissao <= '20230801'
    and Vlr_DescConced > '0.0'
    and ct.[Status] = 'A'
GROUP BY dat_emissao
order by mes

