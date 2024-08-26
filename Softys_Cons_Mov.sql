declare @MES int = 7

SELECT 
	DISTINCT
		'Vendas',
		QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
		VlrBasDscVen = format(Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br')
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
															(cb.Ser_Nota = it.Ser_Nota) AND 
															(cb.Num_Nota = it.Num_Nota)) 
	INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
	left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
	left join FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 3
AND FB.Codigo = 564
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND year(cb.Dat_Emissao) = 2024
AND month(cb.Dat_Emissao) = @MES


SELECT 
	'devolucao',
	sum(Qtd_Itens),
	format(sum(Vlr_Mercadoria), 'c', 'pt-br')
FROM NFECB CB
	inner join NFEIT IT ON CB.Cod_Estabe = IT.Cod_Estabe and cb.Protocolo = it.Protocolo
	INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo
where cb.Cod_Estabe = 3
AND year(Dat_Entrada) = 2024
AND MONTH(Dat_Entrada) = @MES
AND pr.Cod_Fabricante = 564
AND Str_RelDoc like '1,%'



SELECT 
		DISTINCT
		'Bonificacao',
			QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
			VlrBasDscVen = format(Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br')
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
															(cb.Ser_Nota = it.Ser_Nota) AND 
															(cb.Num_Nota = it.Num_Nota)) 
	INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
	left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
	left join FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 3
AND FB.Codigo = 564
			AND year(CB.Dat_Saida) = '2024'
			AND month(CB.Dat_Saida) = @MES
AND IT.Cod_Cfo in (5910, 6910)
AND cb.Status = 'F'  


SELECT 
DISTINCT
	'cancelamento',
	QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
	VlrBasDscVen = format(Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)), 'c', 'pt-br')
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
															(cb.Ser_Nota = it.Ser_Nota) AND 
															(cb.Num_Nota = it.Num_Nota)) 
	INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
	left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
	left join FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 3
AND FB.Codigo = 564
			AND year(CB.Dat_Cancelamento) = 2024
			AND month(CB.Dat_Cancelamento) = @MES
AND cb.Status = 'C'


