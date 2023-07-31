select distinct
    DATENAME (MONTH, GETDATE())  as Mes,
    sum(ct.Vlr_Documento),
    sum(ct.Vlr_DescConced)
	from CTREC ct
		inner join CLIEN cl on ct.Cod_Cliente = cl.Codigo
where ct.cod_estabe = 1
    and dat_emissao >= '20230101' 
    and Dat_Emissao <= '20230131'
    and Vlr_DescConced > '0.0'
    and ct.[Status] = 'A'


