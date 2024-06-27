USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_FATU_DIAS]    Script Date: 27/06/2024 13:24:52 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[DASH_FATU_DIAS] AS
SELECT
VLR_F_30 = (SELECT 
	VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
	FROM PROD_2023.dbo.NFSCB CB 
		INNER JOIN PROD_2023.dbo.NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
	WHERE CB.Cod_Estabe = 1
		AND CB.Tip_Saida = 'V'
		AND Dat_Emissao < GETDATE() - 1
		AND Dat_Emissao >= GETDATE() - 30),

VLR_F_60 = (SELECT 
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
FROM PROD_2023.dbo.NFSCB CB 
	INNER JOIN PROD_2023.dbo.NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
WHERE CB.Cod_Estabe = 1
	AND CB.Tip_Saida = 'V'
	AND Dat_Emissao < GETDATE() - 1
	AND Dat_Emissao >= GETDATE() - 60),


VLR_F_90 = (SELECT 
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
FROM PROD_2023.dbo.NFSCB CB 
	INNER JOIN PROD_2023.dbo.NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
WHERE CB.Cod_Estabe = 1
	AND CB.Tip_Saida = 'V'
	AND Dat_Emissao < GETDATE() - 1
	AND Dat_Emissao >= GETDATE() - 90),

VLR_F_120 = (SELECT 
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
FROM PROD_2023.dbo.NFSCB CB 
	INNER JOIN PROD_2023.dbo.NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
WHERE CB.Cod_Estabe = 1
	AND CB.Tip_Saida = 'V'
	AND Dat_Emissao < GETDATE() - 1
	AND Dat_Emissao >= GETDATE() - 120),


VLR_F_180 = (SELECT 
VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
FROM PROD_2023.dbo.NFSCB CB 
	INNER JOIN PROD_2023.dbo.NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
WHERE CB.Cod_Estabe = 1
	AND CB.Tip_Saida = 'V'
	AND Dat_Emissao < GETDATE() - 1
	AND Dat_Emissao >= GETDATE() - 180)
GO


