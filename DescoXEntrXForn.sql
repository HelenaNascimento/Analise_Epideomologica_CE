select 
    sum(Val_Docume) as Val_Docume,
    sum(val_DscCon) as val_DscCon,
    sum(BX.Val_Princi) as Val_Princi 
    from 
        PAGCT CT
            inner join PAGBX BX ON CT.Cod_Estabe = BX.Cod_Estabe AND CT.Cod_CtaPag = BX.Cod_CtaPag
WHERE CT.COD_ESTABE = 1 
    AND CT.Sta_Docume = 'Q'
    AND CT.Cod_Fornec = 139
    AND Dat_Quitac >= '20230101'
    AND Dat_Quitac <= '20231130'