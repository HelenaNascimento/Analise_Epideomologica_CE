USE [DMD]
GO

/****** Object:  View [dbo].[AnaSaiFabABC]    Script Date: 24/03/2025 12:36:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER view [dbo].[AnaSaiFabABC] as

SELECT 
DISTINCT
pr.Cod_Fabricante, 
fb.Fantasia,
pr.cod_ean,
pr.codigo,
pr.descricao,
pc.Cod_PolCom,
cb.dat_emissao,
QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
VlrFatVen =  Sum(it.Vlr_LiqItem-it.Vlr_RecSbt),
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
VlrBasDsc = Sum(it.Vlr_LiqItem-it.Vlr_RecSbt-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
VlrVen1 = Sum((it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))*(1-IsNull(it.Per_DescontoFin,0)/100)),
VlrVen2 = Sum((it.Vlr_LiqItem-it.Vlr_RecSbt-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))*(1-IsNull(it.Per_DescontoFin,0)/100))
FROM NFSCB cb 
	INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
                                                            (cb.Ser_Nota = it.Ser_Nota) AND 
                                                            (cb.Num_Nota = it.Num_Nota)) 
   INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
   left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
   left join FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 1
AND pr.Cod_Fabricante in (158,319,123,321,588,338,33,237,164,1022)
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND cb.Dat_Emissao >= '20230101' 
AND cb.Dat_Emissao <= '20231130'
Group by 
pr.Cod_Fabricante, 
fb.Fantasia,
pr.cod_ean,
pr.codigo,
pr.descricao,
pc.Cod_PolCom,
cb.dat_emissao
GO


