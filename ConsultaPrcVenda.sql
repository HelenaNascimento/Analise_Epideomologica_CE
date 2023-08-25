select distinct
    pr.codigo,
    pr.descri,
    fb.Fantasia,
    es.Qtd_Dispon,
    format(pr.Prc_Fabric18, 'c', 'pt-br') as 'Preço Fabrica',
    ppr.Per_Descon,
    Prec_Final =  format((Prc_Fabric18 - (Prc_Fabric18 * (Per_Descon/100))), 'c', 'pt-br')
/*
    Des_Esca = 
        case 
            when ppr.Qtd_Min2 > 0 then 'SIM'
            else 'NAO'
        end,
    ppr.Id_PolCom as "Política"
    */
    from PRODU pr
        INNER JOIN PRXES es on pr.codigo = es.cod_produt
        INNER JOIN PCXPR ppr on pr.codigo = ppr.Cod_Produt
        INNER JOIN FABRI fb on pr.Cod_Fabricante = fb.Codigo
        INNER JOIN POCOM pc on ppr.Id_PolCom = pc.Id_PolCom
        INNER JOIN PCXES pes on pc.Id_PolCom = pes.Id_PolCom and es.Cod_Estabe = pes.Cod_Estabe
where es.cod_estabe = 1
    and ppr.Id_PolCom in (2662 ) 
    and fb.codigo in (158, 1022)
    and es.Flg_Bloqueado = 0
    and es.Flg_BlqVen = 0
    and es.Flg_BlqCmp = 0
    and pr.Prc_Fabric18 > 0
    and es.Qtd_Dispon > 0
    and pc.Bloqueado = 0
    and (pc.Dat_Termino = '' or pc.Dat_Termino is not null or pc.Dat_Cadastro <= getdate())
order by pr.codigo

--Eurofarma
--Essity
--OL 001 
-- Cod: 2662

--Outro
--OL 002
-- cod: 2854
