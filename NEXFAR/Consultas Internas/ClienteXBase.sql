SELECT DISTINCT 
	clien.codigo                       AS "clientId",
    pcxes.cod_estabe                   AS "branchId",
    pcxes.cod_estabe                   AS "warehouseId",
    pocom.id_polcom                    AS "priceGroupId",
    pcxve.cod_vended                   AS "sellerId",
    1                                  AS "mainSeller",
    'NEXON'                            AS "module",
    clien.cgc_cpf                      AS "cnpj",
    '1/2/3/4/197/294'                  AS "paymentConditions",
    pocom.vlr_minimo                   AS "orderMinValue",
    CASE
      WHEN clien.cgf = '' THEN 1
      ELSE 0
    END                                AS "taxFree",
    CASE
      WHEN pcxes.cod_estabe = 0 THEN '85'
      WHEN pcxes.cod_estabe = 1 THEN '88'
      WHEN pcxes.cod_estabe = 3 THEN '87'
      WHEN pcxes.cod_estabe = 4 THEN '80'
    END                                "clientTaxCode",
    CONVERT(VARCHAR, enxes.cod_transp) AS "ep_CodTransportadora",
    CONVERT(VARCHAR, enxes.cod_rota)   AS "ep_CodRota",
    CONVERT(VARCHAR, enxes.cod_agecob) AS "ep_CodAgente"
FROM   clien
       INNER JOIN pcxcl
               ON pcxcl.cod_client = clien.codigo
       INNER JOIN pocom
               ON pcxcl.id_polcom = pocom.id_polcom
       INNER JOIN pcxes
               ON pcxcl.id_polcom = pcxes.id_polcom
       INNER JOIN pcxve
               ON pcxcl.id_polcom = pcxve.id_polcom
       JOIN enxes
         ON pcxes.cod_estabe = enxes.cod_estabe
            AND clien.cgc_cpf = enxes.num_cgccpf
WHERE  pocom.id_polcom IN( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate()
       AND clien.pessoa = 'J'
       AND clien.cod_estado IN ( 'BA', 'CE', 'PE' )
       AND clien.flg_blqprm = 0 