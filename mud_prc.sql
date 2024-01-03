select 
    Cod_EAN,
    pr.codigo,
    PR.Descri,
    ES.Prc_Venda,
    ES.Prc_Fabric,
    ES.Prc_MaxCon,
    PR.Prc_Fabric20,
    PR.Prc_MaxCon20,
    Cod_Estabe
    from PRXES ES
        inner join PRODU PR on es.cod_produt = pr.codigo
where cod_estabe in (1)
    and cod_fabricante = 158
    and cod_produt = 10795
    and Tipo = 'R'
    and Flag_ImprClassif1 <> 'N'
    and Qtd_Dispon > 0
    and Prc_Fabric20 > 0;
