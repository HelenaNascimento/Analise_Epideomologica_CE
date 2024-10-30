SELECT DISTINCT prxes.cod_estabe AS "branchId",
                prxes.cod_produt AS "productId",
                prxes.cod_produt AS "sku"
FROM   prxes
       INNER JOIN pcxpr
               ON prxes.cod_produt = pcxpr.cod_produt
       INNER JOIN pocom
               ON pcxpr.id_polcom = pocom.id_polcom
       INNER JOIN estab
               ON prxes.cod_estabe = estab.cod_estabe
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate () 

/*
SELECT
	ES.Cod_Estabe,
	COUNT(PR.Codigo)
	--PR.Codigo
FROM PRODU PR
	JOIN PRXES ES ON PR.Codigo = ES.Cod_Produt
	JOIN PCXPR PP ON ES.Cod_Produt = PP.Cod_Produt
	JOIN POCOM PC ON PP.Id_PolCom = PC.Id_PolCom
WHERE Tipo = 00
AND PC.Id_PolCom IN (3005, 3015, 3004, 3003)
AND PC.Dat_Termino > GETDATE ()
GROUP BY ES.Cod_Estabe
*/