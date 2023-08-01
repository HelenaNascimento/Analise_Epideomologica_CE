/*SELECT  * FROM PRSLD
where cod_estabe = 1 
    and cod_produt = 21786 
    and DATEPART(month,Dat_Movime)= 7*/


SELECT TOP 1
    Cod_Produt as 'Codigo',
    Descri as 'Descricao',
    pr.Unidade_Venda as 'UND',
    substring(fb.Fantasia, 1,10) as 'Fabricante',
    format(Prc_UltEnt, 'c', 'pt-br') as 'Pr Ult Ent',
    format(Prc_CusMedPra, 'c', 'pt-br') as 'Pr. Medio',
    format(Prc_Venda, 'c', 'pt-br') as 'Prc Venda',
    (Qtd_Fisico - (Qtd_Quaren + Qtd_Reserv)) as 'Est Disponivel',
    format(((Qtd_Fisico - (Qtd_Quaren + Qtd_Reserv)) * Prc_CusMedPra), 'c', 'pt-br') as  'Total'
    FROM PRXES es
        inner join PRODU pr on es.Cod_Produt = pr.Codigo 
        inner join FABRI fb on pr.cod_fabricante = fb.Codigo 
where Cod_Estabe = 1
    and  Qtd_Dispon > 0
    and es.Flg_Bloqueado = 0
    and es.Flg_BlqVen = 0
    and es.Flg_BlqCmp = 0
