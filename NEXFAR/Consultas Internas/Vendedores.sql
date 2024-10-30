SELECT DISTINCT vende.nome_guerra AS "name",
                pcxve.cod_vended  AS "externalId",
                vende.cpf         AS "cpf",
                0                 AS "televendas",
                0                 AS "isSupervisor",
                CASE
                  WHEN vende.bloqueado = 0 THEN 'ACTIVE'
                  ELSE 'INACTIVE'
                END               AS "status"
FROM   pcxve
       INNER JOIN pocom
               ON pcxve.id_polcom = pocom.id_polcom
       INNER JOIN vende
               ON pcxve.cod_vended = vende.codigo
       INNER JOIN super
               ON vende.cod_supervisor = super.codigo
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate () 