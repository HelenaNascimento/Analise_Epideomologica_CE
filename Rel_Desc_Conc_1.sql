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
    format(ct.Vlr_Documento, 'c', 'pt-br') as VlrDocumento,
    format(ct.Vlr_DescConced, 'c', 'pt-br') as VlrDescontoConcedido,
	format(IsNull(bx.Vlr_Juros, 0), 'c', 'pt-br') as VlrJuros,
    IsNull(bx.Qtd_DiasAtraso, 0) as Qtd_DiasAtraso,
	format((ct.Vlr_Documento - ct.Vlr_DescConced ), 'c', 'pt-br') as '(Vlr_Doc - Vlr_DescCon)',
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
    and ct.Transacao >= '20220101' 
	and ct.Transacao < '20230101' 
	--and ct.usuario = 'DENICE'
    and Vlr_DescConced > 0
    and ct.[Status] <> 'C'

order by Data_Transacao