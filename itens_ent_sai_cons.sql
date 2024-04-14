select * from NFSIT it
inner join NFSCB cb on 
            it.Cod_Estabe = cb.Cod_Estabe 
            and it.Ser_Nota = cb.Ser_Nota 
            and cb.Num_Nota = it.Num_Nota
where it.Cod_Estabe = 1 and Cod_Produto = 10234 and cb.Dat_Emissao >= '20240213'


select * from NFEIT it
inner join NFeCB cb on 
            it.Cod_Estabe = cb.Cod_Estabe 
            and cb.protocolo = it.protocolo
where it.Cod_Estabe = 1 and Cod_Produto = 10234 and cb.Dat_Entrada >= '20240213'


select * from FS_R0205


update FS_R0205
SET Cod_Estabe = 1


