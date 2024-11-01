USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_HIST_VENDEDOR]    Script Date: 13/04/2024 16:27:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO







CREATE VIEW [dbo].[VW_HIST_VENDEDOR] AS

SELECT 
DISTINCT 
	 cb.Cod_Pedido
	,cb.Num_Nota
	,cb.Cod_Cliente
	,year(cb.Dat_Emissao) as Ano
	,month(cb.Dat_Emissao) as Mes
	,cb.Cod_Vendedor 
	,VEND.Nome_Guerra as Vendedor
	,VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
FROM PROD_2023.DBO.NFSCB CB
	inner join PROD_2023.DBO.NFSIT IT ON CB.Cod_Estabe = it.Cod_Estabe and cb.Ser_Nota = it.Ser_Nota and cb.num_nota = it.Num_Nota
	left join PROD_2023.DBO.VENDE VEND ON CB.Cod_Vendedor = VEND.Codigo
WHERE cb.Cod_Estabe = 1
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND year(cb.Dat_Emissao) >= '2023'
AND month(cb.Dat_Emissao) >= '01'

group by 
	 cb.Cod_Pedido
	,cb.num_nota
	,cb.Cod_Cliente
	,year(cb.Dat_Emissao)
	,month(cb.Dat_Emissao)
	,cb.Cod_Vendedor
	,VEND.Nome_Guerra

GO


