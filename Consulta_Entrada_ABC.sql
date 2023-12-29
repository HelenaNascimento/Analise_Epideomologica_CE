/*
select 
    distinct
    IT.COD_CFO
    from NFECB CB
        inner join  NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
        inner join PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
where CB.cod_estabe = 1
    and CB.Dat_Entrada >= '20230101'
    and CB.DAT_ENTRADA <= '20231130'
    and pr.cod_fabricante = 1022
    */

select 
    'FAT',
    pr.Cod_Fabricante,
    pr.codigo,
    pr.Cod_EAN,
    pr.Descri,
    es.Prc_Fabric,
    es.Prc_CusMedCom,
    es.Prc_UltEnt,
    sum(it.Qtd_PedFat) as Qtd_PedFat,
    '' as Qtd_PedBoni
    from NFECB cb
        inner join NFEIT it on cb.cod_estabe = it.cod_estabe and cb.protocolo = it.protocolo
        inner join PRXES es on it.Cod_Estabe = es.Cod_Estabe and it.Cod_Produto = es.Cod_Produt 
        left join PRODU pr on es.Cod_Produt = pr.Codigo
where 
    cb.cod_estabe = 1
and Cod_EmiFornec = 349
and dat_entrada >= '20230101'
and dat_entrada <= '20231130'
and pr.Cod_Fabricante = 1022
and Cod_Produt = 16803
and it.Cod_Cfo in (2102, 2403, 2404, 2910)

group by 
    pr.Cod_Fabricante,
    pr.codigo,
    pr.Cod_EAN,
    pr.Descri,
    es.Prc_Fabric,
    es.Prc_UltEnt,
    es.Prc_CusMedCom

UNION ALL

select 
    'BON',
    pr.Cod_Fabricante,
    pr.codigo,
    pr.Cod_EAN,
    pr.Descri,
    es.Prc_Fabric,
    es.Prc_CusMedCom,
    es.Prc_UltEnt,
    '',
    sum(it.Qtd_PedFat)   
    from NFECB cb
        inner join NFEIT it on cb.cod_estabe = it.cod_estabe and cb.protocolo = it.protocolo
        inner join PRXES es on it.Cod_Estabe = es.Cod_Estabe and it.Cod_Produto = es.Cod_Produt 
        left join PRODU pr on es.Cod_Produt = pr.Codigo
where 
    cb.cod_estabe = 1
and Cod_EmiFornec = 349
and dat_entrada >= '20230101'
and dat_entrada <= '20231130'
and pr.Cod_Fabricante = 1022
and it.Cod_Cfo in (2910) --2910

group by 
    pr.Cod_Fabricante,
    pr.codigo,
    pr.Cod_EAN,
    pr.Descri,
    es.Prc_Fabric,
    es.Prc_UltEnt,
    es.Prc_CusMedCom

ORDER BY 3


select * from PRXES
where cod_estabe = 1 and cod_produt = 16083