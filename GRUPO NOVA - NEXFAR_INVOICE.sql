SELECT DISTINCT
     PCXES.Cod_Estabe,
    CONCAT(CTREC.Num_Documento, CTREC.Par_Documento, CTREC.Cod_Documento, PCXES.Cod_Estabe, CTREC.Cod_Pedido) AS "externalId",
    CTREC.Cod_Pedido AS "orderExternalId",
    CTREC.Cod_Cliente AS "clientId",
    CTREC.Cod_Estabe AS "branchId",
    CTREC.Cod_Estabe AS "warehouseId",
    CLIEN.Cgc_Cpf AS "cnpj",
    CONVERT(VARCHAR, CTREC.Dat_Emissao, 103) AS "emissionDate",
    CONVERT(VARCHAR, CTREC.Dat_Vencimento, 103) AS "dueDate",
    CASE 
            WHEN CTREC.Par_Documento = 'A' THEN  '1'
            WHEN CTREC.Par_Documento = 'B' THEN  '2'
            WHEN CTREC.Par_Documento = 'C' THEN  '3'
            WHEN CTREC.Par_Documento = 'D' THEN  '4'
            WHEN CTREC.Par_Documento = 'E' THEN  '5'
            WHEN CTREC.Par_Documento = 'F' THEN  '6'
            WHEN CTREC.Par_Documento = 'G' THEN  '7'
            WHEN CTREC.Par_Documento = 'H' THEN  '8'
            WHEN CTREC.Par_Documento = 'I' THEN  '10'
            WHEN CTREC.Par_Documento = 'J' THEN  '11'
            WHEN CTREC.Par_Documento = 'K' THEN  '12'
            WHEN CTREC.Par_Documento = 'L' THEN  '13'
            WHEN CTREC.Par_Documento = 'M' THEN  '14'
            WHEN CTREC.Par_Documento = 'N' THEN  '15'
            WHEN CTREC.Par_Documento = 'O' THEN  '16'
            WHEN CTREC.Par_Documento = 'P' THEN  '17'
            WHEN CTREC.Par_Documento = 'Q' THEN  '18'
            WHEN CTREC.Par_Documento = 'R' THEN  '19'
            WHEN CTREC.Par_Documento = 'S' THEN  '20'
            WHEN CTREC.Par_Documento = 'T' THEN  '21'
            WHEN CTREC.Par_Documento = 'U' THEN  '22'
            WHEN CTREC.Par_Documento = 'V' THEN  '23'
            WHEN CTREC.Par_Documento = 'W' THEN  '24'
    END AS "instalment",
    CTREC.Num_Documento AS "nfeNumber",
    CTREC.Vlr_Documento AS "value",
    CASE 
        WHEN CTREC.Dat_Vencimento < GETDATE () AND CTREC.Status <> 'Q' THEN 'EXPIRED'
        WHEN CTREC.Dat_Vencimento > GETDATE () AND CTREC.Status <> 'Q' THEN 'PENDING'
        WHEN CTREC.Status = 'Q' THEN 'PAID'
    END AS "status"
FROM
    CTREC
    INNER JOIN CLIEN ON CTREC.Cod_Cliente = CLIEN.Codigo
    INNER JOIN PCXCL ON PCXCL.Cod_Client = CLIEN.Codigo
    INNER JOIN POCOM ON PCXCL.Id_PolCom = POCOM.Id_PolCom
    INNER JOIN PCXES ON PCXCL.Id_PolCom = PCXES.Id_PolCom
WHERE
    POCOM.Flg_Web = 1
    AND POCOM.Dat_Termino > GETDATE ()
    AND CLIEN.Pessoa = 'J'
    AND CLIEN.Cod_Estado IN ('BA', 'CE', 'PE')
    AND YEAR (CTREC.Dat_Emissao) > 2022
    AND CTREC.Status <> 'Q'
    AND CTREC.Status <> 'C'
    AND CTREC.Num_Documento IS NOT NULL
	AND CTREC.Par_Documento IS NOT NULL
    AND CTREC.Num_Documento = '1168765'