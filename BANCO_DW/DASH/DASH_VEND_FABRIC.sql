USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_VEND_FABRIC]    Script Date: 11/04/2024 17:16:09 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[DASH_VEND_FABRIC] AS

SELECT 
DISTINCT
pr.Cod_Fabricante, 
fb.Fantasia,
Month(Dat_Emissao) as Mes,
Year(Dat_Emissao) as Ano,
QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
FROM PROD_2023.dbo.NFSCB cb 
	INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
                                                            (cb.Ser_Nota = it.Ser_Nota) AND 
                                                            (cb.Num_Nota = it.Num_Nota)) 
   INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo 
   left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 1
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND cb.Dat_Emissao >= '20230101' 

Group by 
pr.Cod_Fabricante,
fb.Fantasia,
Dat_Emissao

GO


