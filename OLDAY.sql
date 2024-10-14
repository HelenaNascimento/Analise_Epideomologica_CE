SELECT 

DISTINCT
	pr.codigo,
	pr.cod_ean,
	pr.descricao,
	pc.Cod_PolCom,
	pr.Cod_Fabricante,
	fb.Fantasia,
	--Auxiliar = concat(pc.Cod_PolCom, '-', pr.codigo),
	QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
	VlrBasDscVen = convert(decimal(10,2),Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))),
	cb.Dat_Emissao
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
															(cb.Ser_Nota = it.Ser_Nota) AND 
															(cb.Num_Nota = it.Num_Nota)) 
	INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
	left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
	left join FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 1
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND cb.Dat_Emissao >= '20240926'
Group by 
pr.cod_ean,
pr.codigo,
pr.descricao,
pc.Cod_PolCom,
pr.Cod_Fabricante,
fb.Fantasia,
cb.Dat_Emissao
order by 9