SELECT 
    IT.Cod_Pedido AS 'orderExternalId',
    IT.cod_Pedido AS 'transientOrderExternalId',
    IT.Cod_Produto AS 'sku',
    IT.Qtd_Solicitado as 'quantityRequested',
    CONVERT(int, IT.Qtd_ImpFat) AS 'quantityAttended',
    CAST((100 - IT.Per_Desconto) / 100 * IT.Prc_Unitario AS NUMERIC(10,2)) AS 'price'
FROM PDVCB CB 
    INNER JOIN PDVIT IT ON CB.COD_ESTABE = IT.COD_ESTABE
where cb.Cod_PedCli = '129524926819688'