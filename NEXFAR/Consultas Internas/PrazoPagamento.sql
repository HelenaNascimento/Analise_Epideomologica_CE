SELECT DISTINCT przcb.descri_prz AS "name",
                przcb.cod_prz    AS "externalId",
                'BOLETO'         AS "method",
                przcb.qtd_przmed AS "mediumTerm",
                przcb.qtd_parprz AS "instalments",
                CASE
                  WHEN przcb.descri_prz = '45 (30-60)' THEN '30/60'
                  WHEN przcb.descri_prz = '30 (30)' THEN '30'
                  WHEN przcb.descri_prz = 'A VISTA' THEN '7'
                  WHEN przcb.descri_prz = '60 (60)' THEN '60'
                  WHEN przcb.descri_prz = '60 (30-60-90)' THEN '30/60/90'
                  WHEN przcb.descri_prz = '60 (45-60-75)' THEN '45/60/75'
                END              "timesInDays",
                150              AS "orderMinValue"
FROM   przcb
       INNER JOIN pcxpz
               ON przcb.cod_prz = pcxpz.cod_tabprz
       INNER JOIN pocom
               ON pcxpz.id_polcom = pocom.id_polcom
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate ()
       AND pocom.vlr_minimo <> 0.00
       AND przcb.cod_prz IN ( 1, 2, 3, 4,
                              197, 294 ) 