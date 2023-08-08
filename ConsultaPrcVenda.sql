select 
    pr.codigo,
    pr.descri,
    es.Qtd_Dispon,
    format(pr.Prc_Fabric18, 'c', 'pt-br') as 'Preço Fabrica',
    ppr.Per_Descon,
    Prec_Final =  format((Prc_Fabric18 - (Prc_Fabric18 * (Per_Descon/100))), 'c', 'pt-br'),
    Des_Esca = 
        case 
            when ppr.Qtd_Min2 > 0 then 'SIM'
            else 'NAO'
        end
    from PRODU pr
        INNER JOIN PRXES es on pr.codigo = es.cod_produt
        INNER JOIN PCXPR ppr on pr.codigo = ppr.Cod_Produt
where es.cod_estabe = 1
    and ppr.Id_PolCom = 2854
    and es.Flg_Bloqueado = 0
    and es.Flg_BlqVen = 0
    and es.Flg_BlqCmp = 0
    and pr.Prc_Fabric18 > 0
    and es.Qtd_Dispon > 0
order by pr.codigo

--Eurofarma
--Essity
--01 

--Outro
--02
