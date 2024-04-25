SELECT
    CONCAT(V_RGSBT.Cod_RegTri, '_', V_RGSBT.Cod_ClaTri) AS "productTaxCode",
    CASE WHEN V_RGSBT.Cod_PrcBasCreSbt = 'X' THEN
        0
    ELSE
        V_RGSBT.Alq_CreSbt
    END AS "icmsCreditAliquot",
    V_RGSBT.Alq_DebSbt AS "icmsStDebitAliquot"
FROM
    V_RGSBT
WHERE
    V_RGSBT.Cod_RegTri IN (36, 40, 60, 80)