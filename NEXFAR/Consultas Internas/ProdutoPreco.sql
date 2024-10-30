SELECT DISTINCT pcxpr.id_polcom
                AS "priceGroupId"
                ,
                pcxpr.cod_produt
                AS "productId",
                pcxpr.cod_produt
                AS "sku",
                CONVERT(DECIMAL(15, 2), ( prxes.prc_venda - (
                                          pcxpr.per_descon / 100 ) *
                                                            prxes.prc_venda ))
                AS "price",
                1
                AS "allowCreditDown",
                1
                AS "allowCreditUp",
                1
                AS "allowNegotiationDown",
                1
                AS "allowNegotiationUp",
                1
                AS "allowPromotions",
                pcxpr.per_descon
                AS "discount",
                0.01
                AS "minPrice"
FROM   pocom
       INNER JOIN pcxpr
               ON pocom.id_polcom = pcxpr.id_polcom
       INNER JOIN pcxes
               ON pocom.id_polcom = pcxes.id_polcom
       INNER JOIN prxes
               ON pcxpr.cod_produt = prxes.cod_produt
                  AND prxes.cod_estabe = pcxes.cod_estabe
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate ()
       AND prxes.prc_venda > 0
       AND pcxpr.per_descon < 100 

/*
SELECT 
	ES.Cod_Estabe,
	COUNT(PR.Codigo)

FROM   PRODU PR
       INNER JOIN PRXES ES
               ON PR.CODIGO = ES.Cod_Produt
	   JOIN PCXES PS ON ES.Cod_Estabe = PS.Cod_Estabe		
       INNER JOIN PCXPR PP
               ON ES.Cod_Produt = PP.Cod_Produt AND PS.Id_PolCom = PP.Id_PolCom
       INNER JOIN POCOM PC
               ON PP.Id_PolCom = PC.Id_PolCom
WHERE 
	Tipo = 00
	AND pc.id_polcom IN ( 3005, 3015, 3004, 3003 )
    AND pc.dat_termino > Getdate ()
    AND es.prc_venda > 0
    AND pp.per_descon < 100 
GROUP BY ES.Cod_Estabe

*/