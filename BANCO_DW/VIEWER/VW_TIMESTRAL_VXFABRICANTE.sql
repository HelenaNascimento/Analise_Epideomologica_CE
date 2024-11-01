USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_TIMESTRAL_VXFABRICANTE]    Script Date: 13/04/2024 16:54:45 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


			
CREATE 	VIEW [dbo].[VW_TIMESTRAL_VXFABRICANTE] AS		
			SELECT 
			DISTINCT

				FB.CODIGO,
				FB.FANTASIA,
				QTD_VENDAS = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
				VLR_VENDA = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
				M_VENDAS = (Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))/COUNT(CB.NUM_NOTA)),
				year(cb.Dat_Emissao) as Ano, 
				month(cb.Dat_Emissao) as Mes
			FROM PROD_2023.dbo.NFSCB cb 
				INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
				left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
			WHERE cb.Cod_Estabe = 1

			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			AND year(cb.Dat_Emissao) = year(getdate())
			AND month(cb.Dat_Emissao) >= month(getdate()) -3
			Group by 
				FB.CODIGO,
				fb.FANTASIA,
				Dat_Emissao
	
GO


