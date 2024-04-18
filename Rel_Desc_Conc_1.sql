
select 
	distinct
	cl.Razao_Social,
    cl.Cgc_Cpf,
    ct.Num_Documento,
	format (Dat_Emissao, 'd', 'en-gb') as 'Dat_Emissao',
    'Data_Transacao' =
    CASE
        WHEN  ct.Transacao > Dat_Emissao then  format(ct.Transacao, 'd', 'en-gb')
        WHEN  ct.Transacao <= Dat_Emissao then  format(Dat_Emissao, 'd', 'en-gb')
    end,
    ct.Par_Documento,
    ct.[Status],
    CONVERT(DECIMAL(10,2), ct.Vlr_Documento) as VlrDocumento,
    CONVERT(DECIMAL(10,2),ct.Vlr_DescConced) as VlrDescontoConcedido,
	IsNull(bx.Vlr_Juros, 0) as VlrJuros,
    IsNull(bx.Qtd_DiasAtraso, 0) as Qtd_DiasAtraso,
	(ct.Vlr_Documento - ct.Vlr_DescConced ) as '(Vlr_Doc - Vlr_DescCon)',
	IsNull(format(((ct.Vlr_Documento + Isnull(bx.Vlr_Juros, 0)) - (ct.Vlr_DescConced)), 'c', 'pt-br'), 0) as ValorFinal
	from CTREC ct
		left outer join CLIEN cl on ct.Cod_Cliente = cl.Codigo
		left outer join (select 
							Cod_Estabe, 
							Cod_Documento,
							Vlr_Juros,
							Qtd_DiasAtraso
						from bxrec where cod_Estabe = 1) bx on ct.cod_estabe = bx.Cod_Estabe and ct.cod_Documento = bx.cod_Documento
where ct.cod_estabe = 1
    and year(ct.Transacao) = '2024' 
	and month(ct.Transacao) <= month(getdate()) -1
    and Vlr_DescConced > 0
    and ct.[Status] <> 'C'

order by Data_Transacao