SELECT 
        DISTINCT 
        Concat(ctrec.num_documento, ctrec.par_documento,
                ctrec.cod_documento,
                ctrec.cod_estabe, ctrec.cod_pedido) AS  "externalId",
                ctrec.cod_pedido                    AS  "orderExternalId",
                ctrec.cod_cliente                   AS  "clientId",
                ctrec.cod_estabe                    AS  "branchId",
                ctrec.cod_estabe                    AS  "warehouseId",
                clien.cgc_cpf                       AS "cnpj",
                CONVERT(VARCHAR, ctrec.dat_emissao, 103) AS "emissionDate",
                CONVERT(VARCHAR, ctrec.dat_vencimento, 103) AS "dueDate",
                CASE
                  WHEN ctrec.par_documento IN( '', 'A' ) THEN '1'
                  WHEN ctrec.par_documento = 'B' THEN '2'
                  WHEN ctrec.par_documento = 'C' THEN '3'
                  WHEN ctrec.par_documento = 'D' THEN '4'
                  WHEN ctrec.par_documento = 'E' THEN '5'
                  WHEN ctrec.par_documento = 'F' THEN '6'
                  WHEN ctrec.par_documento = 'G' THEN '7'
                  WHEN ctrec.par_documento = 'H' THEN '8'
                  WHEN ctrec.par_documento = 'I' THEN '9'
                  WHEN ctrec.par_documento = 'J' THEN '10'
                  WHEN ctrec.par_documento = 'K' THEN '11'
                  WHEN ctrec.par_documento = 'L' THEN '12'
                  WHEN ctrec.par_documento = 'M' THEN '13'
                  WHEN ctrec.par_documento = 'N' THEN '14'
                  WHEN ctrec.par_documento = 'O' THEN '15'
                  WHEN ctrec.par_documento = 'P' THEN '16'
                  WHEN ctrec.par_documento = 'Q' THEN '17'
                  WHEN ctrec.par_documento = 'R' THEN '18'
                  WHEN ctrec.par_documento = 'S' THEN '19'
                  WHEN ctrec.par_documento = 'T' THEN '20'
                  WHEN ctrec.par_documento = 'U' THEN '21'
                  WHEN ctrec.par_documento = 'V' THEN '22'
                  WHEN ctrec.par_documento = 'W' THEN '23'
                  WHEN ctrec.par_documento = 'X' THEN '24'
                  WHEN ctrec.par_documento = 'Y' THEN '25'
                  WHEN ctrec.par_documento = 'Z' THEN '26'
                END                                                 AS
                "instalment",
                ctrec.num_documento                                 AS
                "nfeNumber",
                ctrec.vlr_documento                                 AS "value",
                CASE
                  WHEN ctrec.dat_vencimento < Getdate ()
                       AND ctrec.status <> 'Q' THEN 'EXPIRED'
                  WHEN ctrec.dat_vencimento > Getdate ()
                       AND ctrec.status <> 'Q' THEN 'PENDING'
                  WHEN ctrec.status = 'Q' THEN 'PAID'
                END                                                 AS "status"
FROM   ctrec
       INNER JOIN clien
               ON ctrec.cod_cliente = clien.codigo
       INNER JOIN pcxcl
               ON pcxcl.cod_client = clien.codigo
       INNER JOIN pocom
               ON pcxcl.id_polcom = pocom.id_polcom
       INNER JOIN pcxes
               ON pcxcl.id_polcom = pcxes.id_polcom
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate ()
       AND clien.pessoa = 'J'
       AND clien.cod_estado IN ( 'BA', 'CE', 'PE' )
       AND Year (ctrec.dat_emissao) > 2022
       AND ctrec.status <> 'Q'
       AND ctrec.status <> 'D'
       AND ctrec.status <> 'C'
       AND ctrec.num_documento IS NOT NULL
       AND ctrec.par_documento IS NOT NULL 