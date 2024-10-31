SELECT DISTINCT
    CONCAT(CTREC.Num_Documento, CTREC.Par_Documento, CTREC.Cod_Documento, CTREC.Cod_Estabe, CTREC.Cod_Pedido) AS "externalId",
    CTREC.Cod_Pedido AS "orderExternalId",
    CTREC.Cod_Cliente AS "clientId",
    CTREC.Cod_Estabe AS "branchId",
    CTREC.Cod_Estabe AS "warehouseId",
    CLIEN.Cgc_Cpf AS "cnpj",
    CONVERT(VARCHAR, CTREC.Dat_Emissao, 103) AS "emissionDate",
    CONVERT(VARCHAR, CTREC.Dat_Vencimento, 103) AS "dueDate",
    CASE WHEN CTREC.Par_Documento IN ('','A') THEN
        '1'
    WHEN CTREC.Par_Documento = 'B' THEN
        '2'
    WHEN CTREC.Par_Documento = 'C' THEN
        '3'
    WHEN CTREC.Par_Documento = 'D' THEN
        '4'
    WHEN CTREC.Par_Documento = 'E' THEN
        '5'
    WHEN CTREC.Par_Documento = 'F' THEN
        '6'
    WHEN CTREC.Par_Documento = 'G' THEN
        '7'
    WHEN CTREC.Par_Documento = 'H' THEN
        '8'
    WHEN CTREC.Par_Documento = 'I' THEN
        '9'
    WHEN CTREC.Par_Documento = 'J' THEN
        '10'
    WHEN CTREC.Par_Documento = 'K' THEN
        '11'
    WHEN CTREC.Par_Documento = 'L' THEN
        '12'
    WHEN CTREC.Par_Documento = 'M' THEN
        '13'
    WHEN CTREC.Par_Documento = 'N' THEN
        '14'
    WHEN CTREC.Par_Documento = 'O' THEN
        '15'
    WHEN CTREC.Par_Documento = 'P' THEN
        '16'
    WHEN CTREC.Par_Documento = 'Q' THEN
        '17'
    WHEN CTREC.Par_Documento = 'R' THEN
        '18'
    WHEN CTREC.Par_Documento = 'S' THEN
        '19'
    WHEN CTREC.Par_Documento = 'T' THEN
        '20'
    WHEN CTREC.Par_Documento = 'U' THEN
        '21'
    WHEN CTREC.Par_Documento = 'V' THEN
        '22'
    WHEN CTREC.Par_Documento = 'W' THEN
        '23'
    WHEN CTREC.Par_Documento = 'X' THEN
        '24'
    WHEN CTREC.Par_Documento = 'Y' THEN
        '25'
    WHEN CTREC.Par_Documento = 'Z' THEN
        '26'
    END AS "instalment",
    CTREC.Num_Documento AS "nfeNumber",
    CTREC.Vlr_Documento AS "value",
    CASE WHEN CTREC.Dat_Vencimento < GETDATE () AND CTREC.Status <> 'Q' THEN
        'EXPIRED'
    WHEN CTREC.Dat_Vencimento > GETDATE () AND CTREC.Status <> 'Q' THEN
        'PENDING'
    WHEN CTREC.Status = 'Q' THEN
        'PAID'
    END AS "status"
FROM
    CTREC
    INNER JOIN CLIEN ON CTREC.Cod_Cliente = CLIEN.Codigo
    INNER JOIN PCXCL ON PCXCL.Cod_Client = CLIEN.Codigo
    INNER JOIN POCOM ON PCXCL.Id_PolCom = POCOM.Id_PolCom
    INNER JOIN PCXES ON PCXCL.Id_PolCom = PCXES.Id_PolCom
WHERE
    	POCOM.id_PolCom IN (3005, 3015, 3004, 3003)
    AND POCOM.Dat_Termino > GETDATE ()
    AND CLIEN.Pessoa = 'J'
    AND CLIEN.Cod_Estado IN ('BA', 'CE', 'PE')
    AND YEAR (CTREC.Dat_Emissao) > 2022
    AND CTREC.Status <> 'Q'
	AND CTREC.Status <> 'D'
    AND CTREC.Status <> 'C'
    AND CTREC.Num_Documento IS NOT NULL
	AND CTREC.Par_Documento IS NOT NULL
	--AND CTREC.Cod_Pedido = '50139981' <-- Colocar nesse campo o número do pedido