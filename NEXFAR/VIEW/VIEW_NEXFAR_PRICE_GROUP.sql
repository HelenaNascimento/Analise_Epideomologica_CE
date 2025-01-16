CREATE VIEW VIEW_NEXFAR_PRICE_GROUP AS

SELECT POCOM.Cod_PolCom AS "name",
       POCOM.Id_PolCom AS "externalId"
FROM POCOM
WHERE POCOM.id_PolCom IN (3005,
                          3015,
                          3004,
                          3003)
  AND POCOM.Dat_Termino > GETDATE ()