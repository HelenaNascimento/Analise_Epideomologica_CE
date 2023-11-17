SELECT 
DISTINCT
pr.Cod_Fabricante, 
pr.cod_ean,
pr.codigo,
pr.descricao,
--Prc_Unitario = format(it.Vlr_LiqItem, 'c', 'pt-br'),
--it.id_polcom,
pc.Cod_PolCom,
QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
VlrFatVen =  FORMAT(Sum(it.Vlr_LiqItem-it.Vlr_RecSbt), 'c', 'pt-br'),
VlrBasDscVen = FORMAT(Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br'),
VlrBasDscVen = FORMAT(Sum(it.Vlr_LiqItem-it.Vlr_RecSbt-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br'),
VlrVen = FORMAT(Sum((it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))*(1-IsNull(it.Per_DescontoFin,0)/100)), 'c', 'pt-br'),
VlrVen = FORMAT(Sum((it.Vlr_LiqItem-it.Vlr_RecSbt-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))*(1-IsNull(it.Per_DescontoFin,0)/100)), 'c', 'pt-br'),
VlrVen = FORMAT(Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br'),
VlrVen =FORMAT(Sum(it.Vlr_LiqItem-it.Vlr_RecSbt-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br')
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
                                                            (cb.Ser_Nota = it.Ser_Nota) AND 
                                                            (cb.Num_Nota = it.Num_Nota)) 
   INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
   left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
WHERE cb.Cod_Estabe = 1
AND pr.Cod_Fabricante = 164
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND cb.Dat_Emissao >= '20230401' 
AND cb.Dat_Emissao <= '20230930'


Group by 
	pr.Cod_Fabricante, 
	pr.cod_ean,
	pr.codigo,
	pr.descricao,
	pc.Cod_PolCom

order by 2,4