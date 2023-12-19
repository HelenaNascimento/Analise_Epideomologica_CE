select 
format(SUM(Val_Docume), 'c', 'pt-br') as Val_Docume,
format(SUM(Val_DscCon), 'c', 'pt-br') as Val_DscCon,
--format(SUM(Val_Docume) - SUM(Val_DscCon), 'c', 'pt-br') as Final
format(sum(bx.Val_Princi), 'c', 'pt-br') as Val_Princi
from PAGCT ct
    inner join PAGBX bx on ct.Cod_Estabe = bx.Cod_Estabe and  ct.Cod_CtaPag = bx.Cod_CtaPag
where ct.cod_estabe = 1 
    and Cod_Fornec = 336
    and Sta_Docume = 'Q'
    and ct.Dat_Vencim >= '20230101'
    and ct.Dat_Vencim <= '20231130'

