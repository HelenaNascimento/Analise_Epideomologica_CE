Select 
    format(sum (Vlr_Mercadoria), 'c', 'pt-br') as Vlr_Mercadoria,
    format(sum(Vlr_Desconto), 'c', 'pt-br') as Vlr_Desconto,
    format(sum(Vlr_Nota), 'c', 'pt-br')  as Vlr_Nota
     from NFECB
where cod_estabe = 1  
    and Dat_Entrada >= '20230101'
    and Dat_Entrada <= '20231130'
    and Cod_EmiFornec = 349
    and Tip_NF <> 'D'
    and Status NOT IN ('A', 'C')


