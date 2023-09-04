/*SELECT  * FROM PRSLD
where cod_estabe = 1 
    and cod_produt = 21786 
    and DATEPART(month,Dat_Movime)= 7*/


SELECT 
    Cod_Produt as 'Codigo',
    TRIM(Descricao) as 'Descricao',
    TRIM(pr.Unidade_Venda) as 'UND',
    substring(fb.Fantasia, 1,10) as 'Fabricante',
    'Pr Ult Ent' = case
                        when (Prc_CusLiqEnt) > 0 then format(Prc_CusLiqEnt, 'c', 'pt-br')
                        when (Prc_CusLiqEnt) <= 0 then format(Prc_UltEnt, 'c', 'pt-br')
                end ,
    format(Prc_CusMedPra, 'c', 'pt-br') as 'Pr. Medio',
    format(Prc_Venda, 'c', 'pt-br') as 'Prc Venda',
    (Qtd_Fisico) as 'Est Disponivel',
    format(((Qtd_Fisico - (Qtd_Quaren + Qtd_Reserv)) * Prc_CusMedPra), 'c', 'pt-br') as  'Total'
    FROM PRXES es
        inner join PRODU pr on es.Cod_Produt = pr.Codigo 
        inner join FABRI fb on pr.cod_fabricante = fb.Codigo 
where Cod_Estabe = 1
    and  Qtd_Dispon > 0
    and es.Flg_Bloqueado = 0
    and es.Flg_BlqVen = 0
    and es.Flg_BlqCmp = 0
    and pr.Flag_ImprClassif1 <> 'N'
 --   and pr.Codigo = 22264

