SELECT DISTINCT produ.descricao                        AS "name",
                pcxpr.cod_produt                       AS "sku",
                produ.cod_ean                          AS "ean",
                fabri.fantasia                         AS "maker",
                class.descricao                        AS "category",
                produ.des_nomgen                       AS "similar",
                produ.des_prdref                       AS "reference",
                produ.descri                           AS
                "complementaryDescription",
                produ.qtd_embalagem                    AS "boxQuantity",
                1                                      AS "availableNexOn",
                CASE
                  WHEN produ.tip_por344 = '' THEN 0
                  ELSE 1
                END                                    AS "controlled",
                CASE
                  WHEN produ.tip_por344 = '' THEN 0
                  ELSE 1
                END                                    AS
                "documentationRequired",
                1                                      AS "active",
                CONVERT(VARCHAR, produ.cod_fabricante) AS "ep_CodFabricante"
FROM   pcxpr
       INNER JOIN produ
               ON pcxpr.cod_produt = produ.codigo
       INNER JOIN fabri
               ON produ.cod_fabricante = fabri.codigo
       INNER JOIN class
               ON produ.cod_classif = class.codigo
       INNER JOIN pocom
               ON pcxpr.id_polcom = pocom.id_polcom
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 ) 