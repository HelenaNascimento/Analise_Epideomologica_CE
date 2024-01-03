
ROLLBACK

BEGIN
UPDATE ES
SET Prc_Venda = PF_20,
    Prc_Fabric = PF_20,
    Prc_MaxCon = PMC_20
from PRXES ES 
    INNER JOIN PRODU PR ON ES.Cod_Produt = PR.Codigo
    INNER JOIN ABCIT IT ON PR.Cod_EAN = IT.EAN
where Cod_Estabe = 1
    and cod_produt = 10795
    and pr.Prc_Fabric18 = IT.PF_18