SELECT
    CONCAT(V_RGSBT.Cod_RegTri, '_', V_RGSBT.Cod_ClaTri) AS "productTaxCode",
    V_RGSBT.Per_LimiteSTMin AS "minimumLimitPercentageFactorIcmsSt"
FROM
    V_RGSBT
WHERE
    V_RGSBT.Cod_RegTri IN (36, 40, 60, 80)