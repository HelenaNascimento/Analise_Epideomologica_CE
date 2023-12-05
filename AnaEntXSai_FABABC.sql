declare @CodFab int = 588, @datin date = '20230101', @datf date ='20231031'

select 
	distinct
	SAI.Cod_Fabricante,
	SAI.cod_ean,
	SAI.codigo,
	SAI.descricao,
	ENT.prc_venda,
	--ENT.protocolo,
	ENT.Prc_UniFat,
	ENT.Qtd_Comp,
	sai.Cod_PolCom,
	sum(SAI.QtdVen),
	sum(SAI.VlrVen1),
	sum(SAI.VlrVen2)
	from AnaSaiFabABC SAI
			left outer join (SELECT
								distinct
								Cod_Fabricante, 
								cod_ean, 
								codigo, 
								descricao, 
								prc_venda, 
								--protocolo, 
								Prc_UniFat, 
								SUM(Qtd_Comp) AS Qtd_Comp  
						FROM AnaEntFabABC
						WHERE Cod_Fabricante = @CodFab
						group by
								Cod_Fabricante, 
								cod_ean, 
								codigo, 
								descricao, 
								prc_venda, 
								--protocolo, 
								Prc_UniFat) ENT ON ENT.Cod_Fabricante = SAI.Cod_Fabricante   AND ENT.cod_ean = SAI.cod_ean 
where
     ent.Cod_Fabricante = @CodFab
 --and ent.dat_entrada >= '20230101'
 --and ent.dat_entrada <= '20231031'
 and SAI.dat_emissao >= @datin
 and SAI.dat_emissao <= @datf

 group by 
	SAI.Cod_Fabricante,
	SAI.cod_ean,
	SAI.codigo,
	SAI.descricao,
	ENT.prc_venda,
	ENT.Qtd_Comp,
	ENT.Prc_UniFat,
	sai.Cod_PolCom
 order by 3, 10

