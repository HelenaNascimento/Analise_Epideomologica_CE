USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_TRIMESTRAL_VXCLIENTES]    Script Date: 13/04/2024 17:00:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_TRIMESTRAL_VXCLIENTES] AS
			SELECT 
			DISTINCT
				cl.Codigo,
				SUBSTRING(CL.Razao_Social, 0, 20) AS Razao_Social,
				QTD_VENDAS = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
				VLR_VENDA = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
				M_VENDAS = (Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))/COUNT(CB.NUM_NOTA)),
				month(cb.Dat_Emissao) as Mes,
				year(cb.Dat_Emissao) as Ano 
				
			FROM PROD_2023.dbo.NFSCB cb 
				INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
				left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join PROD_2023.dbo.CLIEN CL on cb.Cod_Cliente = cl.Codigo
				
			WHERE cb.Cod_Estabe = 1

			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			AND year(cb.Dat_Emissao) = year(getdate())
			AND month(cb.Dat_Emissao) >= month(getdate()) -3
			Group by 
				CL.Codigo,
				CL.Razao_Social,
				Dat_Emissao
GO


