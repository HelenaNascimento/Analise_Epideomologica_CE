SELECT pocom.cod_polcom AS "name",
       pocom.id_polcom  AS "externalId",
       'ACTIVE'         AS "status"
FROM   pocom
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate () 