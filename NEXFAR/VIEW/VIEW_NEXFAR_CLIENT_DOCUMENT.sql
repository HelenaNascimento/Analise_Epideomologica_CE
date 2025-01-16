CREATE VIEW VIEW_NEXFAR_CLIENT_DOCUMENT AS 

SELECT DISTINCT CLIEN.Codigo AS "clientId",
                CLIEN.cgc_cpf AS "cnpj",
                CASE
                    WHEN CLIEN.Num_Anvisa IS NOT NULL THEN 'ANVISA'
                    WHEN CLIEN.Num_CerReg IS NOT NULL THEN 'CRF'
                    WHEN CLIEN.Num_AlvFun IS NOT NULL THEN 'VISA'
                    WHEN CLIEN.Licenca_Saude IS NOT NULL THEN 'CONTROLADO'
                    ELSE ''
                END AS "type",
                CASE
                    WHEN CLIEN.Val_Anvisa < GETDATE()
                         OR CLIEN.Val_CerReg < GETDATE()
                         OR CLIEN.Val_AlvFun < GETDATE()
                         OR CLIEN.Val_LicSau < GETDATE() THEN 'VENCIDO'
                    WHEN CLIEN.Val_Anvisa >= GETDATE()
                         OR CLIEN.Val_CerReg >= GETDATE()
                         OR CLIEN.Val_AlvFun >= GETDATE()
                         OR CLIEN.Val_LicSau >= GETDATE() THEN 'REGULAR'
                    ELSE ''
                END AS "status"
FROM CLIEN
INNER JOIN PCXCL ON PCXCL.COD_CLIENT = CLIEN.Codigo
INNER JOIN POCOM ON PCXCL.Id_PolCom = POCOM.Id_PolCom
WHERE POCOM.id_PolCom IN(3005,
                         3015,
                         3004,
                         3003)
  AND POCOM.Dat_Termino > GETDATE ()
  AND CLIEN.Pessoa = 'J'
  AND CLIEN.Cod_Estado IN ('BA',
                           'CE',
                           'PE')
  AND CLIEN.Flg_BlqPrm = 0
  AND (CLIEN.Num_Anvisa IS NOT NULL
       OR CLIEN.Num_CerReg IS NOT NULL
       OR CLIEN.Num_AlvFun IS NOT NULL
       OR CLIEN.Licenca_Saude IS NOT NULL)
  AND (CLIEN.Val_Anvisa IS NOT NULL
       OR CLIEN.Val_CerReg IS NOT NULL
       OR CLIEN.Val_AlvFun IS NOT NULL
       OR CLIEN.Val_LicSau IS NOT NULL)