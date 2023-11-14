
create view SANDOZ_OL2 AS

SELECT  distinct
	PR.CODIGO,
	PR.COD_EAN,
	PR.DESCRICAO,
	Lis = CASE 
				WHEN PR.Tip_LisPis = 'P' THEN '( + )'
				WHEN PR.Tip_LisPis = 'N' THEN '( - )'
				WHEN PR.Tip_LisPis = 'X' THEN '( n )'
	end,
	es.Cod_ClaTri,
	Fabricante = (select Fantasia from fabri where codigo = 164),
	format(ent.Prc_UniFat, 'c', 'pt-br') as 'DANFE',
	format(dat_movi, 'd', 'en-gb') as Dat_Ent,
	'ICMS' = format((ent.Prc_UniFat / 100 * 13.71), 'c', 'pt-br'),
	'C.FIXO' = format(((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*7.0864), 'c', 'pt-br'),
	'C.VENDA' = format(((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*4), 'c', 'pt-br'),
	'I.FEDERAL' = format((((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*107.8)/100*3.25), 'c', 'pt-br'),
	'INVEST' = format(((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3))/100 * 1), 'c', 'pt-br'),
	'P.C.RESC.' = format(((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*107.8), 'c', 'pt-br'),
	'Markup' = format((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)), 'c', 'pt-br'),
	TOTAL =  format((ent.Prc_UniFat + (ent.Prc_UniFat / 100 * 13.71) + ((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*7.0864) +
			((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*4) + (((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*107.8)/100*3.25) +
			 ((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3))/100 * 1)), 'c', 'pt-br'),
	'P.VENDA' = format(pr.Prc_Fabric18, 'c', 'pt-br'),
	'DESC' = concat(convert(decimal(10,2), ((pr.Prc_Fabric18 - (ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)))/pr.Prc_Fabric18)*100), '%'),
	'L.LIQ' = format((((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*107.8) - (ent.Prc_UniFat + (ent.Prc_UniFat / 100 * 13.71) + ((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*7.0864) +
			((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*4) + (((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*107.8)/100*3.25) +
			 ((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3))/100 * 1)))/((ent.Prc_UniFat+(ent.Prc_UniFat * 0.3)) / 100*107.8)*100, 'c', 'pt-br'),
	'Est.Dispo' = es.Qtd_Dispon

	FROM POCOM PC
		INNER JOIN PCXPR PPC ON PC.Id_PolCom = PPC.Id_PolCom
		INNER JOIN PRODU PR ON PPC.Cod_Produt = PR.Codigo
		INNER JOIN PRXES ES ON PR.CODIGO = ES.Cod_Produt  
		LEFT OUTER JOIN (SELECT distinct IT.Cod_Estabe, Cod_Produto, Prc_UniFat, max(Dat_Movime) as dat_movi
							FROM NFEIT IT
								INNER JOIN PRSLD PD ON IT.Cod_Estabe = PD.Cod_Estabe AND IT.Cod_Produto = PD.Cod_Produt
							WHERE IT.Cod_Estabe = 1 AND pd.Dat_Movime > '20221231' and pd.Dat_Movime < '20231101'
							group by IT.Cod_Estabe, Cod_Produto, Prc_UniFat) ent on 
							es.Cod_Estabe = ent.Cod_Estabe and pr.codigo = ent.Cod_Produto
WHERE es.Cod_Estabe = 1
and PC.Id_PolCom =2854
and pr.Cod_Fabricante = 164
and Flag_ImprClassif1 <> 'N'
--and pr.codigo = 204
group by 
	PR.CODIGO,
	PR.COD_EAN,
	PR.DESCRICAO,
	PR.Tip_LisPis,
	es.Cod_ClaTri,
	ent.Prc_UniFat,
	dat_movi,
	pr.Prc_Fabric18,
	Qtd_Dispon

HAVING(dat_movi) < '20231101'
