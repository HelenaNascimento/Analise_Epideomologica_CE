USE [DMD_TESTE]
GO

/****** Object:  View [dbo].[VW_REL_ABC_FAB_SAIVEND]    Script Date: 01/07/2025 09:38:43 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE VIEW [dbo].[VW_REL_ABC_FAB_SAIVEND] AS 
SELECT 
DISTINCT
	PR.codigo AS Codigo,
	PR.cod_ean AS EAN,
	PR.descricao AS Produto,
	pc.Cod_PolCom AS Politica,
	Soma_Qtd_Venda = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
	Soma_Valor_Venda = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
	year(cb.Dat_Emissao) AS Ano_Emissao,
	month(cb.Dat_Emissao) Mes_Emissao,
	Fantasia AS Fabricante
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
															(cb.Ser_Nota = it.Ser_Nota) AND 
															(cb.Num_Nota = it.Num_Nota)) 
	INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
	left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
	left join FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 1
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND year(cb.Dat_Emissao) > YEAR(GETDATE()) -1
Group by 
pr.cod_ean,
pr.codigo,
pr.descricao,
pc.Cod_PolCom,
CB.Dat_Emissao,
Fantasia




GO


