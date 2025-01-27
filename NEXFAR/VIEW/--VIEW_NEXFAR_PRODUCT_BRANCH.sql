CREATE VIEW VIEW_NEXFAR_PRODUCT_BRANCH AS

SELECT DISTINCT prxes.cod_produt AS "productId",
                prxes.cod_estabe AS "branchId",
                prxes.cod_produt AS "sku"
FROM PRODU
JOIN PRXES ON PRODU.Codigo = PRXES.Cod_Produt
JOIN PCXPR ON PRXES.Cod_Produt = PCXPR.Cod_Produt
JOIN POCOM ON PCXPR.Id_PolCom = POCOM.Id_PolCom
WHERE Tipo = 00
  AND POCOM.Id_PolCom IN(3005,
                         3015,
                         3004,
                         3003)
  AND POCOM.Dat_Termino > GETDATE ()