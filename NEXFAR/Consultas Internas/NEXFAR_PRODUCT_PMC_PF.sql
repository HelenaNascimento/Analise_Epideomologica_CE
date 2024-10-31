SELECT DISTINCT produ.codigo AS "productId",
                produ.codigo AS "sku",
                CASE
                  WHEN pcxes.cod_estabe = 0 THEN 'PE'
                  WHEN pcxes.cod_estabe = 1 THEN 'CE'
                  WHEN pcxes.cod_estabe = 3 THEN 'PE'
                  WHEN pcxes.cod_estabe = 4 THEN 'BA'
                END          AS "uf",
                CASE
                  WHEN pcxes.cod_estabe = 0
                       AND COALESCE(produ.prc_fabric205, 0) <> 0 THEN
                  produ.prc_fabric205
                  WHEN pcxes.cod_estabe = 1
                       AND COALESCE(produ.prc_fabric20, 0) <> 0 THEN
                  produ.prc_fabric20
                  WHEN pcxes.cod_estabe = 3
                       AND COALESCE(produ.prc_fabric205, 0) <> 0 THEN
                  produ.prc_fabric205
                  WHEN pcxes.cod_estabe = 4
                       AND COALESCE(produ.prc_fabric19, 0) <> 0 THEN
                  produ.prc_fabric19
                  ELSE prxes.prc_venda
                END          AS "industryPrice",
                CASE
                  WHEN pcxes.cod_estabe = 0 THEN produ.prc_maxcon205
                  WHEN pcxes.cod_estabe = 1 THEN produ.prc_maxcon20
                  WHEN pcxes.cod_estabe = 3 THEN produ.prc_maxcon205
                  WHEN pcxes.cod_estabe = 4 THEN produ.prc_maxcon19
                END          AS "pmc"
FROM   produ
       INNER JOIN pcxpr
               ON pcxpr.cod_produt = produ.codigo
       INNER JOIN pocom
               ON pcxpr.id_polcom = pocom.id_polcom
       INNER JOIN pcxes
               ON pcxes.id_polcom = pcxpr.id_polcom
       INNER JOIN prxes
               ON pcxpr.cod_produt = prxes.cod_produt
                  AND prxes.cod_estabe = pcxes.cod_estabe
WHERE  pocom.id_polcom IN( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate () 

/*
--PRODUCT_PMC_PF

SELECT DISTINCT produ.codigo AS "productId",
                produ.codigo AS "sku",
                CASE
                  WHEN pcxes.cod_estabe = 0 THEN 'PE'
                  WHEN pcxes.cod_estabe = 1 THEN 'CE'
                  WHEN pcxes.cod_estabe = 3 THEN 'PE'
                  WHEN pcxes.cod_estabe = 4 THEN 'BA'
                END          AS "uf",
                CASE
                  WHEN pcxes.cod_estabe = 0
                       AND COALESCE(produ.prc_fabric205, 0) <> 0 THEN
                  produ.prc_fabric205
                  WHEN pcxes.cod_estabe = 1
                       AND COALESCE(produ.prc_fabric20, 0) <> 0 THEN
                  produ.prc_fabric20
                  WHEN pcxes.cod_estabe = 3
                       AND COALESCE(produ.prc_fabric205, 0) <> 0 THEN
                  produ.prc_fabric205
                  WHEN pcxes.cod_estabe = 4
                       AND COALESCE(produ.prc_fabric19, 0) <> 0 THEN
                  produ.prc_fabric19
                  ELSE prxes.prc_venda
                END          AS "industryPrice",
                CASE
                  WHEN pcxes.cod_estabe = 0 THEN produ.prc_maxcon205
                  WHEN pcxes.cod_estabe = 1 THEN produ.prc_maxcon20
                  WHEN pcxes.cod_estabe = 3 THEN produ.prc_maxcon205
                  WHEN pcxes.cod_estabe = 4 THEN produ.prc_maxcon19
                END          AS "pmc"
FROM   produ
       INNER JOIN pcxpr
               ON pcxpr.cod_produt = produ.codigo
       INNER JOIN pocom
               ON pcxpr.id_polcom = pocom.id_polcom
       INNER JOIN pcxes
               ON pcxes.id_polcom = pcxpr.id_polcom
       INNER JOIN prxes
               ON pcxpr.cod_produt = prxes.cod_produt
                  AND prxes.cod_estabe = pcxes.cod_estabe
WHERE  TIPO = 00
AND pocom.id_polcom IN( 3005, 3015, 3004, 3003 )
AND pocom.dat_termino > Getdate () 

*/