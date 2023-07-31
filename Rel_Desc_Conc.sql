

select distinct
    cl.Razao_Social,
    cl.Cgc_Cpf,
    ct.Num_Documento,
    format(ct.Dat_Emissao, 'd', 'en-gb') as DatEmissao,
    ct.Par_Documento,
    ct.[Status],
    format(ct.Vlr_Documento, 'c', 'pt-br') as VlrDocumento,
    format(ct.Vlr_DescConced, 'c', 'pt-br') as VlrDescontoConcedido,
    format(bx.Vlr_Desconto, 'c', 'pt-br') as VlrDesconto,
    format(bx.Vlr_Juros, 'c', 'pt-br') as VlrJuros,
    bx.Qtd_DiasAtraso,
    ct.Per_MulAtrPag,
    format(((ct.Vlr_Documento - ct.Vlr_DescConced ) + (bx.Vlr_Juros - bx.Vlr_Desconto)), 'c', 'pt-br') as ValorFinal
    from BXREC bx
        inner join CTREC ct on bx.Cod_Estabe  = ct.Cod_Estabe and bx.[Status] = ct.[Status] and bx.Cod_Documento = ct.Cod_Documento 
        inner join CLIEN cl on ct.Cod_Cliente = cl.Codigo
where ct.cod_estabe = 1
    and DATEPART(year, Dat_Emissao) = 2022 
    and Vlr_DescConced > '0.0'

union all

select 
	cl.Razao_Social,
    cl.Cgc_Cpf,
    ct.Num_Documento,
    format(ct.Dat_Emissao, 'd', 'en-gb') as DatEmissao,
    ct.Par_Documento,
    ct.[Status],
    format(ct.Vlr_Documento, 'c', 'pt-br') as VlrDocumento,
    format(ct.Vlr_DescConced, 'c', 'pt-br') as VlrDescontoConcedido,
	'0.0', 
    '0.0', 
    0, 
    0, 
    format((ct.Vlr_Documento - ct.Vlr_DescConced ), 'c', 'pr-br') 
	from CTREC ct
		inner join CLIEN cl on ct.Cod_Cliente = cl.Codigo
where ct.cod_estabe = 1
   and DATEPART(year, Dat_Emissao) = 2022 
    and Vlr_DescConced > '0.0'
    and ct.[Status] = 'A'

order by  ct.Num_Documento, ct.Par_Documento

