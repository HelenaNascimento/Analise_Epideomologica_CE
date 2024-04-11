USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_FABR]    Script Date: 11/04/2024 17:31:04 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE VIEW [dbo].[VW_FABR] AS
SELECT 
	Codigo, 
	Fantasia, 
	Cgc_Cpf, 
	fb.Sta_ClaAbcVal, 
	fb.Per_ParticFat, 
	Qtd_PrzMaxFat,
	Per_DscMaxVis,
	Per_DscMaxPrz,
	Flg_Desconto,
	Flg_BlqInfPar,
	Per_DscBasComNor,
	Per_DscBasTax,
	Per_ComBasTax,
	Transacao,
	Flg_Exclusivi,
	Flg_InfCnvNfs,
	Bloqueado
FROM PROD_2023.dbo.FABRI fb
	INNER JOIN PROD_2023.dbo.FBXES es on fb.Codigo = es.Cod_Fabric
WHERE ES.Cod_Estabe = 1
GO


