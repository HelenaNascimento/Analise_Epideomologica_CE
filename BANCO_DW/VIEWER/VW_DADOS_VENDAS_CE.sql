USE [DW_PROD]
GO

/****** Object:  View [dbo].[VV_DADOS_VENDAS_CE]    Script Date: 11/04/2024 17:21:00 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


create view [dbo].[VV_DADOS_VENDAS_CE] as 
			SELECT 
			DISTINCT
				fb.Fantasia,
				pr.codigo,
				pr.cod_ean,
				pr.descricao,
				pc.Cod_PolCom,
				QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
				VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
				substring(CL.Fantasia, 0, 20) as Cliente,
				substring(VD.Nome_Guerra, 0, 20) as Vendedor,
				year(cb.Dat_Emissao)as ano,
				month(cb.Dat_Emissao) as mes

			FROM PROD_2023.dbo.NFSCB cb 
				INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
				left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
				left join PROD_2023.dbo.CLIEN CL on cb.Cod_Cliente = CL.Codigo
				left join PROD_2023.dbo.VENDE VD on cb.Cod_Vendedor = VD.Codigo
			WHERE cb.Cod_Estabe = 1
			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			AND year(cb.Dat_Emissao) >= '2023'
			AND month(cb.Dat_Emissao) >= '01'
		Group by 
			fb.Fantasia,
			pr.codigo,
			pr.cod_ean,
			pr.descricao,
			pc.Cod_PolCom,
			CL.Fantasia,
			VD.Nome_Guerra,
			cb.Dat_Emissao,
			cb.Dat_Emissao
GO


