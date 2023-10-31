/*
select TOP 100  * from PRSLD
where cod_estabe = 1 and Cod_Produt = 26


select 
   Cod_Produt,
    Dat_Movime,
    Estoque = 
        Case
            when Qtd_EntCom > 0 or Qtd_EntTrf > 0 or  Qtd_EntDev > 0 or Qtd_EntOut > 0 then Qtd_SldAtu
     from PRSLD
where cod_estabe = 1
*/

select 
    saldoant =
        case 
            when (Qtd_SldAtu > 0 and (sld1.dat - 1) <> null) THEN Qtd_SldAtu
        end
    from PRSLD sld
        inner join (Select 
                        Cod_Estabe, 
                        Cod_Produt, 
                        max(Dat_Movime) as dat  
                    from PRSLD 
                    where 
                        Dat_Movime >= '20230401'
                        and Dat_Movime < = '20230930' 
                    group by Cod_Estabe, 
                        Cod_Produt) sld1 on sld.Cod_Estabe = sld1.Cod_Estabe and sld.Cod_Produt = sld1.Cod_Produt and sld.Dat_Movime = sld1.dat

where sld.Cod_Estabe = 1 
and sld.Cod_Produt = 26 
group by 
    Qtd_SldAtu,
    sld1.dat