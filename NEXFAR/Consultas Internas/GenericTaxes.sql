SELECT Concat(v_rgsbt.cod_regtri, '_', v_rgsbt.cod_clatri) AS "productTaxCode",
       CASE
         WHEN v_rgsbt.cod_prcbascresbt = 'X' THEN 0
         ELSE v_rgsbt.alq_cresbt
       END                                                 AS
       "icmsCreditAliquot",
       v_rgsbt.alq_debsbt                                  AS
       "icmsStDebitAliquot"
FROM   v_rgsbt
WHERE  v_rgsbt.cod_regtri IN( 87, 85, 88, 80 ) 