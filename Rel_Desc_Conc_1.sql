
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
    CONVERT(DECIMAL(10,2),ct.Vlr_DescConced) as Desconto_Financeiro,
	ISNULL(CONVERT(DECIMAL(10,2), ct.Per_DescFinanc), 0) as Desconto_Comercial,
	IsNull(bx.Vlr_Juros, 0) as VlrJuros,
    IsNull(bx.Qtd_DiasAtraso, 0) as Qtd_DiasAtraso,
    '(Vlr_Doc - Vlr_DescCon)' =
                        CASE
                            WHEN IsNull(bx.Qtd_DiasAtraso, 0) > 0 THEN ISNULL((ct.Vlr_Documento - ct.Vlr_DescConced), 0)
                        	WHEN IsNull(bx.Qtd_DiasAtraso, 0) = 0 THEN ISNULL((ct.Vlr_Documento - ct.Vlr_DescConced)-((ct.Vlr_Documento - ct.Vlr_DescConced) * (ct.Per_DescFinanc/100) ), 0)
                        END,
	ValorFinal =
                        CASE
                            WHEN IsNull(bx.Qtd_DiasAtraso, 0) > 0 THEN  IsNull(format(((ct.Vlr_Documento - ct.Vlr_DescConced) + Isnull(bx.Vlr_Juros, 0)), 'c', 'pt-br'), 0)
                            WHEN IsNull(bx.Qtd_DiasAtraso, 0) = 0 THEN  IsNull(format(((((ct.Vlr_Documento - ct.Vlr_DescConced)-((ct.Vlr_Documento - ct.Vlr_DescConced) * (ct.Per_DescFinanc/100) )) + Isnull(bx.Vlr_Juros, 0)) ), 'c', 'pt-br'), 0)
                        END
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
    and (Vlr_DescConced > 0 or ct.Per_DescFinanc > 0)
    and ct.[Status] <> 'C'

order by Data_Transacao