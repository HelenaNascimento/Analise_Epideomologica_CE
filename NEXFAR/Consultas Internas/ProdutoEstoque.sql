SELECT DISTINCT prxes.cod_produt                    AS "productId",
                prxes.cod_estabe                    AS "warehouseId",
                prxes.cod_produt                    AS "sku",
                prxes.qtd_dispon - prxes.qtd_quaren AS "quantityAvailable",
                (SELECT Min(pr1.dat_vencim)
                 FROM   prlot pr1
                 WHERE  pr1.cod_estabe = prxes.cod_estabe
                        AND prxes.cod_produt = pr1.cod_produt
                        AND pr1.dat_vencim >= Getdate () + 180
                        AND pr1.qtd_fisico > 0)     AS "validUntil"
FROM   prxes
       INNER JOIN pcxpr
               ON prxes.cod_produt = pcxpr.cod_produt
       INNER JOIN pocom
               ON pcxpr.id_polcom = pocom.id_polcom
       INNER JOIN estab
               ON prxes.cod_estabe = estab.cod_estabe
       INNER JOIN prlot
               ON prxes.cod_produt = prlot.cod_produt
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate ()
       AND prxes.qtd_dispon > 0
       AND prlot.dat_vencim > Getdate () + 180 

/*
SELECT
	ES.Cod_Estabe,
	COUNT(PR.Codigo)
	--PR.Codigo
FROM PRODU PR
	JOIN PRXES ES ON PR.Codigo = ES.Cod_Produt
	JOIN PRLOT LT ON ES.Cod_Produt = LT.Cod_Produt and es.Cod_Estabe = lt.Cod_Estabe
	JOIN PCXPR PP ON LT.Cod_Produt = PP.Cod_Produt
	JOIN POCOM PC ON PP.Id_PolCom = PC.Id_PolCom
WHERE  Tipo = 00
AND PC.id_polcom IN ( 3005, 3015, 3004, 3003 )
AND PC.dat_termino > Getdate ()
AND ES.qtd_dispon > 0
AND LT.dat_vencim > Getdate () + 180 
GROUP BY ES.Cod_Estabe
*/