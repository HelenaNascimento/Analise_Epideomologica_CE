/*
--Localizar Coluna --

SELECT
    T.name AS Tabela,
    C.name AS Coluna
FROM
    sys.sysobjects    AS T (NOLOCK)
INNER JOIN sys.all_columns AS C (NOLOCK) ON T.id = C.object_id AND T.XTYPE = 'U'
WHERE
    C.NAME LIKE '%Desc%'
ORDER BY
    T.name ASC



select 
    distinct
    it.Cod_Cfo
    from PRODU pr
        inner join NFEIT it on pr.codigo = it.cod_produto
        inner join NFECB cb on it.Cod_Estabe = cb.Cod_Estabe and it.Protocolo = cb.Protocolo 
where it.cod_estabe = 1
    and cb.Dat_Entrada >= '20230101'
    and cb.Dat_Entrada <= '20231130'
    and pr.cod_fabricante = 123
*/
/*
SELECT 
    it.Cod_Produto, 
    pr.descri,
    pr.cod_ean,
    sum(Qtd_PedFat) as Qtd_PedFat, 
    es.Prc_UltEnt,
    es.Prc_Fabric,
    es.Prc_Venda,
    es.Prc_CusMedCom,
    ((sum(it.Prc_CustoMedio)) / (count(it.Prc_CustoMedio))) as Prc_CustoMedio
    FROM NFEIT IT
        inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
        inner join PRODU PR ON IT.Cod_Produto = PR.Codigo
        inner join PRXES ES ON CB.Cod_Estabe = ES.Cod_Estabe AND IT.Cod_Produto = ES.Cod_Produt
WHERE IT.Cod_Estabe = 1
    and pr.Cod_Fabricante = 123
    --and cb.Dat_Entrada >= '20230101'
    --and cb.Dat_Entrada <= '20231130'
    and it.Cod_Produto = 18957
    and IT.Cod_Cfo in ( 2102, 2403, 2404)
group by 
    it.Cod_Produto, 
    pr.descri,
    pr.cod_ean,
    es.Prc_UltEnt,
    es.Prc_Fabric,
    es.Prc_Venda,
    es.Prc_CusMedCom
order by it.Cod_Produto
*/
/*
select ean.Cod_EAN 
    from PREAN ean
        inner join PRXES es on ean.Cod_Produt = es.Cod_Produt
where  
    es.Cod_Estabe = 1
    and ean.Cod_Produt = 4959  
*/

select 
    distinct
    IT.COD_CFO
    from NFECB CB
        inner join  NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
        inner join PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
where CB.cod_estabe = 1
    and CB.Dat_Entrada = '20230101'
    and CB.DAT_ENTRADA = '20231130'
    and CB.Cod_EmiFornec = 233
   -- AND pr.Cod_Fabricante = 237


    --237
    --233