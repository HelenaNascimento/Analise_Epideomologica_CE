CREATE VIEW VIEW_NEXFAR_PAYMENT_CONDITION AS

SELECT DISTINCT PRZCB.Descri_Prz AS "name",
                PRZCB.Cod_Prz AS "externalId",
                'BOLETO' AS "method",
                PRZCB.Qtd_PrzMed AS "mediumTerm",
                PRZCB.Qtd_ParPrz AS "instalments",
                CASE
                    WHEN PRZCB.Descri_Prz = '45 (30-60)' THEN '30/60'
                    WHEN PRZCB.Descri_Prz = '30 (30)' THEN '30'
                    WHEN PRZCB.Descri_Prz = 'A VISTA' THEN '7'
                    WHEN PRZCB.Descri_Prz = '60 (60)' THEN '60'
                    WHEN PRZCB.Descri_Prz = '60 (30-60-90)' THEN '30/60/90'
                    WHEN PRZCB.Descri_Prz = '60 (45-60-75)' THEN '45/60/75'
                END "timesInDays",
                150 AS "orderMinValue"
FROM PRZCB
INNER JOIN PCXPZ ON PRZCB.Cod_Prz = PCXPZ.Cod_TabPrz
INNER JOIN POCOM ON PCXPZ.Id_PolCom = POCOM.Id_PolCom
WHERE POCOM.id_PolCom IN (3005,
                          3015,
                          3004,
                          3003)
  AND POCOM.Dat_Termino > GETDATE ()
  AND POCOM.Vlr_Minimo <> 0.00
  AND PRZCB.Cod_Prz IN (1,
                        2,
                        3,
                        4,
                        197,
                        294)