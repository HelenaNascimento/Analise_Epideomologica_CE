SELECT DISTINCT CLIEN.Codigo AS "clientId",
                PCXES.Cod_Estabe AS "branchId",
                PCXES.Cod_Estabe AS "warehouseId",
                POCOM.Id_PolCom AS "priceGroupId",
                PCXVE.Cod_Vended AS "sellerId",
                1 AS "mainSeller",
                'NEXON' AS "module",
                CLIEN.Cgc_Cpf AS "cnpj",
                '1/2/3/4/197/294' AS "paymentConditions",
                POCOM.Vlr_Minimo AS "orderMinValue",
                CASE
                    WHEN CLIEN.Cgf = '' THEN 1
                    ELSE 0
                END AS "taxFree",
                CASE
                    WHEN PCXES.Cod_Estabe = 0 THEN '40'
                    WHEN PCXES.Cod_Estabe = 1 THEN '60'
                    WHEN PCXES.Cod_Estabe = 3 THEN '36'
                    WHEN PCXES.Cod_Estabe = 4 THEN '80'
                END AS "clientTaxCode",
                CONVERT(VARCHAR, ENXES.Cod_Transp) AS "ep_CodTransportadora",
                CONVERT(VARCHAR, ENXES.Cod_Rota) AS "ep_CodRota",
                CONVERT(VARCHAR, ENXES.Cod_AgeCob) AS "ep_CodAgente"
FROM CLIEN
INNER JOIN PCXCL ON PCXCL.Cod_Client = CLIEN.Codigo
INNER JOIN POCOM ON PCXCL.Id_PolCom = POCOM.Id_PolCom
INNER JOIN PCXES ON PCXCL.Id_PolCom = PCXES.Id_PolCom
INNER JOIN PCXVE ON PCXCL.Id_PolCom = PCXVE.Id_PolCom
JOIN ENXES ON PCXES.Cod_Estabe = ENXES.Cod_Estabe
AND CLIEN.Cgc_Cpf = ENXES.Num_CgcCpf
WHERE CLIEN.Codigo = 16005
  AND POCOM.id_PolCom IN(3005,
                         3015,
                         3004,
                         3003)
  AND POCOM.Dat_Termino > GETDATE ()
  AND CLIEN.Pessoa = 'J'
  AND CLIEN.Cod_Estado IN ('BA',
                           'CE',
                           'PE')
  AND CLIEN.Flg_BlqPrm = 0