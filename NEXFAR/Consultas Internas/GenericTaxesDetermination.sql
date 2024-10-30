SELECT DISTINCT 
  prxes.cod_estabe AS "warehouseId",
  estab.des_estado AS "warehouseUf",
  prxes.cod_produt AS "sku",
  CASE
    WHEN prxes.cod_estabe = 0 THEN '85'
    WHEN prxes.cod_estabe = 1 THEN '88'
    WHEN prxes.cod_estabe = 3 THEN '87'
    WHEN prxes.cod_estabe = 4 THEN '80'
  END              'clientTaxCode',
  CASE
    WHEN prxes.cod_estabe = 0 THEN Concat('85_', prxes.cod_clatri)
    WHEN prxes.cod_estabe = 1 THEN Concat('88_', prxes.cod_clatri)
    WHEN prxes.cod_estabe = 3 THEN Concat('87_', prxes.cod_clatri)
    WHEN prxes.cod_estabe = 4 THEN Concat('80_', prxes.cod_clatri)
  END              'productTaxCode',
  CASE
    WHEN prxes.cod_clatri IN( 'FG', 'FGR', 'FGR2' ) THEN 'PMC'
    WHEN prxes.cod_clatri = 'FS' THEN 'PF'
    WHEN prxes.cod_clatri = 'I' THEN 'ISENTO'
    ELSE 'IVA'
  END              AS 'taxType'
FROM   prxes
       JOIN enxes
         ON enxes.cod_estabe = prxes.cod_estabe
       JOIN estab
         ON estab.cod_estabe = prxes.cod_estabe
       JOIN clien
         ON clien.cgc_cpf = enxes.num_cgccpf
       JOIN v_rgsbt
         ON v_rgsbt.cod_regtri = enxes.cod_regtri
            AND v_rgsbt.cod_clatri = prxes.cod_clatri
       JOIN produ
         ON produ.codigo = prxes.cod_produt
WHERE  prxes.qtd_dispon > 0
       AND enxes.cod_estabe IN ( '0', '1', '3', '4' )
       AND clien.pessoa = 'J'
       AND clien.cod_estado IN ( 'BA', 'CE', 'PE' )
       AND enxes.cod_regtri IS NOT NULL
       AND prxes.cod_clatri IS NOT NULL
       AND v_rgsbt.cod_prcbasdebsbt IS NOT NULL 