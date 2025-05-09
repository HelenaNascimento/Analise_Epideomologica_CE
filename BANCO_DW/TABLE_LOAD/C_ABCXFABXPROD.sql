	INSERT INTO DW_PROD.dbo.[C_ABCXFABXPROD] (Cod_Fabri, Cod_Ean, Codigo, Descri, Cod_PolCom, Auxilixar, Qtd_Vend, VlrFatVen, VlrBasDscVen, Dat_Emissao)
	SELECT 

			distinct
				pr.Cod_Fabricante,
				pr.cod_ean,
				pr.codigo,
				pr.descricao,
				pc.Cod_PolCom,
				Auxiliar = concat(pc.Cod_PolCom, '-', pr.codigo),
				QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),
				VlrFatVen =  Sum(it.Vlr_LiqItem-it.Vlr_RecSbt),
				VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
				cb.Dat_Emissao
			FROM PROD_2023.dbo.NFSCB cb 
				INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
				left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
			WHERE cb.Cod_Estabe = 1
			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			Group by 
				pr.Cod_Fabricante,
				pr.cod_ean,
				pr.codigo,
				pr.descricao,
				pc.Cod_PolCom,
				cb.Dat_Emissao
 