CREATE VIEW VIEW_NEXFAR_PRODUCT_PRICE AS

SELECT DISTINCT pcxpr.cod_produt AS "productId",
                pcxpr.id_polcom AS "priceGroupId",
                pcxpr.cod_produt AS "sku",
                CONVERT(DECIMAL(15, 2),(prxes.prc_venda - (pcxpr.per_descon / 100) * prxes.prc_venda)) AS "price",
                1 AS "allowPromotions",
                pcxpr.per_descon AS "discount",
                0.01 AS "minPrice"
FROM PRODU
INNER JOIN PRXES ON PRODU.CODIGO = PRXES.Cod_Produt
JOIN PCXES ON PRXES.Cod_Estabe = PCXES.Cod_Estabe
INNER JOIN PCXPR ON PRXES.Cod_Produt = PCXPR.Cod_Produt
AND PCXES.Id_PolCom = PCXPR.Id_PolCom
INNER JOIN POCOM ON PCXPR.Id_PolCom = POCOM.Id_PolCom
WHERE Tipo = 00
  AND POCOM.id_polcom IN (3005,
                          3015,
                          3004,
                          3003)
  AND POCOM.dat_termino > Getdate ()
  AND PRXES.prc_venda > 0
  AND PCXPR.per_descon < 100