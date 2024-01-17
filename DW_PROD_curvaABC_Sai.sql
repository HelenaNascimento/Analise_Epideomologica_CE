USE DW_PROD
GO
/*
CREATE TABLE C_ABCXFABXPROD (
			Cod_Fabri int,
			Cod_Ean varchar(14),
			Codigo int,
			Descri varchar(80),
			Cod_PolCom varchar(50),
			Auxilixar varchar(80),
			Qtd_Vend int,
			VlrFatVen decimal(20,4),
			VlrBasDscVen decimal(20,4),
			Mes_Fat varchar(4)


)

*/


INSERT INTO C_ABCXFABXPROD 
SELECT 
DISTINCT
pr.Cod_Fabricante, 
pr.cod_ean,
pr.codigo,
pr.descricao,
pc.Cod_PolCom,
CONCAT(pc.Cod_PolCom, '-', pr.codigo) AS AUXILIAR,
QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
VlrFatVen =  Sum(it.Vlr_LiqItem-it.Vlr_RecSbt),
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
Dat_Emissao = 
	CASE 
		WHEN Dat_Emissao >= '20230101' AND Dat_Emissao <= '20230131' THEN 'JAN'
		WHEN Dat_Emissao >= '20230201' AND Dat_Emissao <= '20230228' THEN 'FEV'
		WHEN Dat_Emissao >= '20230301' AND Dat_Emissao <= '20230331' THEN 'MAR'
		WHEN Dat_Emissao >= '20230401' AND Dat_Emissao <= '20230430' THEN 'ABR'
		WHEN Dat_Emissao >= '20230501' AND Dat_Emissao <= '20230531' THEN 'MAI'
		WHEN Dat_Emissao >= '20230601' AND Dat_Emissao <= '20230630' THEN 'JUN'
		WHEN Dat_Emissao >= '20230701' AND Dat_Emissao <= '20230731' THEN 'JUL'
		WHEN Dat_Emissao >= '20230801' AND Dat_Emissao <= '20230831' THEN 'AGO'
		WHEN Dat_Emissao >= '20230901' AND Dat_Emissao <= '20230930' THEN 'SET'
		WHEN Dat_Emissao >= '20231001' AND Dat_Emissao <= '20231031' THEN 'OUT'
		WHEN Dat_Emissao >= '20231101' AND Dat_Emissao <= '20231130' THEN 'NOV'
		WHEN Dat_Emissao >= '20231201' AND Dat_Emissao <= '20231231' THEN 'DEZ'
	END
FROM PROD_2023.dbo.NFSCB cb 
	INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
                                                            (cb.Ser_Nota = it.Ser_Nota) AND 
                                                            (cb.Num_Nota = it.Num_Nota)) 
   INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
   left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
   left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 1
AND pr.Cod_Fabricante IN (158,319,123,134,321,588,17,636,237,601,1022,33,4,164,348,222,83,96,195,75,69,40,192,276,832,197,386,70,280,98,185,772,541,46,325,286,947,737,268,79,1015,903,575,963)
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND cb.Dat_Emissao >= '20230101' 
AND cb.Dat_Emissao <= '20231231'
Group by 
pr.Cod_Fabricante, 
fb.Fantasia,
pr.cod_ean,
pr.codigo,
pr.descricao,
pc.Cod_PolCom,
Dat_Emissao

order by 3
