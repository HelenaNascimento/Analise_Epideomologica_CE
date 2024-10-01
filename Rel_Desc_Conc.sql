declare @yy int = 2023, @mm int = 08

select distinct
    cl.Razao_Social,
    cl.Cgc_Cpf,
    ct.Num_Documento,
    'Data' =
        CASE
            WHEN  ct.Transacao > Dat_Emissao then  format(ct.Transacao, 'd', 'en-gb')
            WHEN  ct.Transacao <= Dat_Emissao then  format(Dat_Emissao, 'd', 'en-gb')
    end,
    ct.Par_Documento,
    ct.[Status],
    convert(decimal (10,2), (ct.Vlr_Documento)) as VlrDocumento,
    convert(decimal (10,2), ct.Vlr_DescConced) as VlrDescontoConcedido,
    CONVERT(DECIMAL(10,2),ct.Vlr_DspFin) as Desconto,
    convert(decimal (10,2), bx.Vlr_Juros) as VlrJuros,
    bx.Qtd_DiasAtraso,
    ct.Per_MulAtrPag,
    convert(decimal (10,2), ((ct.Vlr_Documento - ct.Vlr_DescConced ) + (bx.Vlr_Juros - bx.Vlr_Desconto))) as ValorFinal
    from BXREC bx
        inner join CTREC ct on bx.Cod_Estabe  = ct.Cod_Estabe and bx.[Status] = ct.[Status] and bx.Cod_Documento = ct.Cod_Documento 
        inner join CLIEN cl on ct.Cod_Cliente = cl.Codigo
where ct.cod_estabe = 1
    and ct.Transacao >= '20230108' 
    and Vlr_DescConced > 0


union all

select 
	cl.Razao_Social,
    cl.Cgc_Cpf,
    ct.Num_Documento,
    'Data' =
    CASE
        WHEN  ct.Transacao > Dat_Emissao then  format(ct.Transacao, 'd', 'en-gb')
        WHEN  ct.Transacao <= Dat_Emissao then  format(Dat_Emissao, 'd', 'en-gb')
    end,
    ct.Par_Documento,
    ct.[Status],
    convert(decimal (10,2), ct.Vlr_Documento) as VlrDocumento,
    convert(decimal (10,2), ct.Vlr_DescConced) as VlrDescontoConcedido,
	--'0.0', 
    '0.0', 
    0, 
    0, 
    convert(decimal (10,2), (ct.Vlr_Documento - ct.Vlr_DescConced ))
	from CTREC ct
		inner join CLIEN cl on ct.Cod_Cliente = cl.Codigo
where ct.cod_estabe = 1
    and ct.Transacao >= '20230108' 
    and Vlr_DescConced > 0
    and ct.[Status] = 'A'

order by  [Data] --, ct.Num_Documento, ct.Par_Documento



