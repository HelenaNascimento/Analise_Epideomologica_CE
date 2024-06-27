USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_RELACAO_ENT_SAI_FAB]    Script Date: 27/06/2024 13:29:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE VIEW [dbo].[DASH_RELACAO_ENT_SAI_FAB] AS
SELECT 
DISTINCT 
	FB.Fantasia,
	PR.Cod_EAN,
	PR.CODIGO,
	PR.Descricao,
	IsNull(PRC_M.Prc_Pr,0) as Prc_Pr,
	IsNull(PRC_M.CM,0) as CM,
	IsNull(ENT.Prc_Unitario,0) as Prc_UltEnt,
	IsNull(ENT.Qtd_Compra, 0) as Qtd_Compra,
	IsNull(ENT.Valor, 0) as Valor,
	PC.Cod_PolCom,
	QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
	VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
	SAI_T.T_QtdVen,
	SAI_T.T_Vlr,
	year(cb.Dat_Emissao) ANO,
	month(cb.Dat_Emissao) MES
FROM PROD_2023.dbo.NFSCB cb 
	INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
															(cb.Ser_Nota = it.Ser_Nota) AND 
															(cb.Num_Nota = it.Num_Nota)) 
	INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
	LEFT JOIN PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
	LEFT JOIN PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
	LEFT JOIN (SELECT distinct
					Cod_Fabricante as Codigo,
					IT.Cod_Produto,
					IT.Prc_Unitario,
					sum(it.Qtd_PedFat) as Qtd_Compra,
					sum((Prc_Unitario * it.Qtd_PedFat)) as Valor,
					month(CB.DAT_ENTRADA) as Mes,
					year(CB.DAT_ENTRADA) as Ano
					FROM PROD_2023.dbo.NFEIT IT
						INNER JOIN PROD_2023.dbo.NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
						INNER JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
						left join PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo			
						WHERE 
						IT.Cod_Estabe = 1
						AND year(CB.DAT_ENTRADA) >= year(getdate()) -1
						AND IT.Cod_Cfo in (1910, 2910, 2102, 2403, 2404) 

						GROUP BY
							Cod_Fabricante,
							IT.Cod_Produto,
							IT.Prc_Unitario,
							CB.DAT_ENTRADA) ENT ON PR.Cod_Fabricante = ENT.Codigo AND PR.Codigo = ENT.Cod_Produto AND year(CB.Dat_Emissao) = ent.Ano and month(cb.Dat_Emissao) = ent.Mes
			LEFT JOIN (SELECT 
							DISTINCT
							[CodProd]
							,[Prc_Pr]
							,[CM]
							,[Ano]
							,[Mes]
							,[Dt_Alt]
						FROM [DW_PROD].[dbo].[V_HSPRC_FAKE_VIEW]) PRC_M ON IT.Cod_Produto = PRC_M.CodProd
			LEFT JOIN(SELECT  
						DISTINCT
						[CodProd]
					   ,[Prc_UltEnt]
					   ,[Mes]
					   ,[Ano]
					   ,[Dt_Alt]
				FROM [DW_PROD].[dbo].[VW_PRC_ITENS_ENTR]) PRC on it.Cod_Produto = prc.CodProd
			LEFT JOIN (SELECT 
							DISTINCT
								PR.CODIGO as P_PROD,
								T_QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
								T_Vlr = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
							FROM PROD_2023.dbo.NFSCB cb 
								INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																						(cb.Ser_Nota = it.Ser_Nota) AND 
																						(cb.Num_Nota = it.Num_Nota)) 
								INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
								left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
								left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
							WHERE cb.Cod_Estabe = 1
							AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
							AND year(cb.Dat_Emissao) >=  year(getdate()) -1
							Group by 
							PR.CODIGO ) SAI_T ON PR.Codigo = SAI_T.P_PROD
WHERE CB.Cod_Estabe = 1
AND PR.Tipo = 'R'
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND year(cb.Dat_Emissao) >=  year(getdate()) -1

group by
	FB.Fantasia,
	PR.Cod_EAN,
	PR.CODIGO,
	PR.Descricao,
	PRC_M.Prc_Pr,
	PRC_M.CM,
	ENT.Prc_Unitario,
	ENT.Qtd_Compra,
	ENT.Valor,
	PC.Cod_PolCom,
	SAI_T.T_QtdVen,
	SAI_T.T_Vlr,
	cb.Dat_Emissao
GO


