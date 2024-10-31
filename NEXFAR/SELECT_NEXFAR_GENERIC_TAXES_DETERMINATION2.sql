SELECT DISTINCT
    PRXES.Cod_Estabe AS "warehouseId",
    ESTAB.Des_Estado AS "warehouseUf",
    PRXES.Cod_Produt AS "sku",
    CASE WHEN PRXES.Cod_Estabe = 0 THEN
        '40'
    WHEN PRXES.Cod_Estabe = 1 THEN
        '60'
    WHEN PRXES.Cod_Estabe = 3 THEN
        '36'
    WHEN PRXES.Cod_Estabe = 4 THEN
        '80'
    END 'clientTaxCode',
    CASE WHEN PRXES.Cod_Estabe = 0 THEN
        CONCAT('85', PRXES.Cod_ClaTri) 
    WHEN PRXES.Cod_Estabe = 1 THEN
        CONCAT('88', PRXES.Cod_ClaTri)
    WHEN PRXES.Cod_Estabe = 3 THEN
        CONCAT('87', PRXES.Cod_ClaTri) 
    WHEN PRXES.Cod_Estabe = 4 THEN
        CONCAT('80', PRXES.Cod_ClaTri)
    END 'productTaxCode',
    CASE WHEN PRXES.Cod_ClaTri IN ('FG', 'FGR', 'FGR2') THEN
        'PMC'
    WHEN PRXES.Cod_ClaTri = 'FS' THEN
        'PF'
    WHEN PRXES.Cod_ClaTri = 'I' THEN
        'ISENTO'
    ELSE
        'IVA'
    END AS 'taxType'
FROM
    PRXES
    JOIN ENXES ON ENXES.Cod_Estabe = PRXES.Cod_Estabe
    JOIN ESTAB ON ESTAB.Cod_Estabe = PRXES.Cod_Estabe
    JOIN CLIEN ON CLIEN.Cgc_Cpf = ENXES.Num_CgcCpf
    JOIN V_RGSBT ON V_RGSBT.Cod_RegTri = ENXES.Cod_RegTri
        AND V_RGSBT.Cod_ClaTri = PRXES.Cod_ClaTri
    JOIN PRODU ON PRODU.Codigo = PRXES.Cod_Produt
WHERE
    PRXES.Qtd_Dispon > 0
    AND ENXES.Cod_Estabe IN ('0', '1', '3', '4')
    AND CLIEN.Pessoa = 'J'
    AND CLIEN.Cod_Estado IN ('BA', 'CE', 'PE')
    AND ENXES.Cod_RegTri IS NOT NULL
    AND PRXES.Cod_ClaTri IS NOT NULL
    AND V_RGSBT.Cod_PrcBasDebSbt IS NOT NULL