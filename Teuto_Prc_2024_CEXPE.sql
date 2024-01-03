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

ROLLBACK
COMMIT

begin TRAN
update ES
set Prc_Venda = PR.Prc_Fabric20,
    Prc_Fabric = PR.Prc_Fabric20,
    Prc_MaxCon = PR.Prc_MaxCon20
from PRXES ES 
    INNER JOIN PRODU PR ON  ES.COD_PRODUT = PR.CODIGO
where Cod_Estabe = 1
    and cod_produt = 10795


SELECT
    T.name AS Tabela,
    C.name AS Coluna
FROM
    sys.sysobjects    AS T (NOLOCK)
INNER JOIN sys.all_columns AS C (NOLOCK) ON T.id = C.object_id AND T.XTYPE = 'U'
WHERE
    C.NAME LIKE '%prc_f%'
ORDER BY
    T.name ASC;

select * from ABCIT

select * from ABCCB

SELECT 
    it.ID_Produto,
    Prc_Venda = PF_20,
    Prc_Fabric = PF_20,
    Prc_MaxCon = PMC_20
from PRXES ES 
    INNER JOIN PRODU PR ON ES.Cod_Produt = PR.Codigo
    INNER JOIN ABCIT IT ON PR.Cod_EAN = IT.EAN
where Cod_Estabe = 1
    and cod_produt = 10795
    and pr.Prc_Fabric18 = IT.PF_18