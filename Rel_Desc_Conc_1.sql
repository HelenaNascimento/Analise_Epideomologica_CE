--alter VIEW llm.VW_Rel_DescConce AS
SELECT  
	distinct
    Estabe = 
        case    
            when ct.Cod_Estabe = 0 then 'NovaPE'
            when ct.Cod_Estabe = 1 then 'NovaCE'
            when ct.Cod_Estabe = 3 then 'NovaMult'
            when ct.Cod_Estabe = 4 then 'NovaBA'
        end,
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
    Format(ct.Vlr_Documento, 'c', 'pt-br') as VlrDocumento,
    Format(ct.Vlr_DescConced, 'c', 'pt-br') as Desconto_Financeiro,
	Format(ct.Per_DescFinanc, 'c', 'pt-br') as Desconto_Comercial,
	Format(Isnull(bx.Vlr_Juros, '0'), 'c', 'pt-br') as VlrJuros,
    IsNull(bx.Qtd_DiasAtraso, 0) as Qtd_DiasAtraso,
    '(Vlr_Doc - Vlr_DescCon)' =
                        CASE
                            WHEN IsNull(bx.Qtd_DiasAtraso, 0) > 0 THEN REPLACE(ISNULL((ct.Vlr_Documento - ct.Vlr_DescConced), 0), '.', ',')
                        	WHEN IsNull(bx.Qtd_DiasAtraso, 0) = 0 THEN REPLACE(ISNULL((ct.Vlr_Documento - ct.Vlr_DescConced)-((ct.Vlr_Documento - ct.Vlr_DescConced) * (ct.Per_DescFinanc/100) ), 0), '.', ',')
                        END,
	ValorFinal =
                        CASE
                            WHEN IsNull(bx.Qtd_DiasAtraso, 0) > 0 THEN  IsNull(format(((ct.Vlr_Documento - ct.Vlr_DescConced) + Isnull(bx.Vlr_Juros, 0)), 'c', 'pt-br'), 0)
                            WHEN IsNull(bx.Qtd_DiasAtraso, 0) = 0 THEN  IsNull(format(((((ct.Vlr_Documento - ct.Vlr_DescConced)-((ct.Vlr_Documento - ct.Vlr_DescConced) * (ct.Per_DescFinanc/100) )) + Isnull(bx.Vlr_Juros, 0)) ), 'c', 'pt-br'), 0)
                        END
	FROM CTREC ct
		left outer join CLIEN cl on ct.Cod_Cliente = cl.Codigo
		left outer join (select 
							Cod_Estabe, 
							Cod_Documento,
							Vlr_Juros,
							Qtd_DiasAtraso
						from bxrec where cod_Estabe = 1) bx on ct.cod_estabe = bx.Cod_Estabe and ct.cod_Documento = bx.cod_Documento
where ct.Transacao >= DATEADD(month, -2, getdate()) 
    and (Vlr_DescConced > 0 or ct.Per_DescFinanc > 0)
    and ct.[Status] <> 'C'