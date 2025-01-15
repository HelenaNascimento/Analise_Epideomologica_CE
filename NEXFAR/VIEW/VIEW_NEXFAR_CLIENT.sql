CREATE VIEW VIEW_NEXFAR_CLIENT AS

SELECT DISTINCT 
    Isnull(CLIEN.Fantasia, 'Não Possui') AS "name",
    CLIEN.Cgc_Cpf AS "cnpj", 
    CLIEN.Codigo AS "externalId", 
    CLIEN.Fone1 AS "phone",
    CLIEN.Cep AS "cep",
    CLIEN.Cod_Estado AS "state",
    CONCAT('NOVA', CLIEN.Cod_Estado) AS "address",
    CASE 
		WHEN CLIEN.Cod_RamoAtividade = 11 THEN 1
		ELSE 0
	END AS drugstore

FROM CLIEN 
    JOIN ENXES ON CLIEN.Codigo = ENXES.Cod_Client AND CLIEN.Cgc_Cpf = ENXES.Num_CgcCpf 
	JOIN PCXCL ON CLIEN.Codigo = PCXCL.Cod_Client
	JOIN POCOM ON PCXCL.Id_PolCom = POCOM.Id_PolCom
WHERE POCOM.id_PolCom IN(3005, 3015, 3004, 3003) 
    AND POCOM.Dat_Termino > GETDATE() 
	AND CLIEN.Bloqueado = 0
    AND CLIEN.Pessoa = 'J' 
    AND CLIEN.Cod_Estado IN ('BA', 'CE', 'PE') 
    AND CLIEN.Flg_BlqPrm = 0
	