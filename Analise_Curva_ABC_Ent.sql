/*
select 
    distinct
    IT.COD_CFO
    from NFECB CB
        inner join  NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
        inner join PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
where CB.cod_estabe = 1
    and CB.Dat_Entrada >= '20230101'
    and CB.DAT_ENTRADA <= '20231130'
    and pr.cod_fabricante = 237
*/


declare
@codEstab int = 1,
@DatIn smalldatetime = '20230101',
@DatFim smalldatetime= '20231130',
@CFABR int = 1022

SELECT
		PRD.Cod_Fabricante,
		FB.Fantasia,
		PRD.CODIGO,
		PRD.Cod_EAN,
		PRD.Descri,
		Prc_UltEnt,
		eit2.Qtd_PedFat as Qtd_Ent_Comp,
		eit1.Qtd_PedFat as Qtd_Ent_BONI,
		Prc_Fabric,
		Prc_Venda,
		Prc_CusMedCom
	FROM PRODU PRD
		INNER JOIN PRXES PES ON PRD.Codigo = PES.Cod_Produt
		INNER JOIN FABRI FB ON PRD.Cod_Fabricante = FB.Codigo 
		INNER  JOIN (SELECT 
						Cod_Produto, 
						sum(Qtd_PedFat) as Qtd_PedFat, 
						IT.Cod_Estabe  
						FROM NFEIT IT
							inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
					WHERE IT.Cod_Estabe = @codEstab
					and cb.Dat_Entrada >= @DatIn
					and cb.Dat_Entrada <= @DatFim
					and IT.Cod_Cfo in (1910, 2910)
					group by Cod_Produto, IT.Cod_Estabe  ) eit1 on PES.Cod_Estabe = eit1.Cod_Estabe and PES.Cod_Produt = eit1.Cod_Produto
					
		INNER JOIN (SELECT 
						Cod_Produto, 
						sum(Qtd_PedFat) as Qtd_PedFat, 
						IT.Cod_Estabe  
						FROM NFEIT IT
							inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
					WHERE IT.Cod_Estabe = @codEstab
					
					and cb.Dat_Entrada >= @DatIn
					and cb.Dat_Entrada <= @DatFim
					and IT.Cod_Cfo in (2102, 2403, 2404)
					group by Cod_Produto, IT.Cod_Estabe) eit2 on PES.Cod_Estabe = eit2.Cod_Estabe and PES.Cod_Produt = eit2.Cod_Produto
	where 
	pes.Cod_Estabe = @codEstab
	and PRD.Cod_Fabricante = @CFABR
	

order by 3


