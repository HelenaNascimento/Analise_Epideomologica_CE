USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_VEND]    Script Date: 13/04/2024 17:02:07 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_VEND] AS
SELECT 
	Codigo,
	Nome_Guerra,
	Nome_Completo,
	Cod_Gerencia,
	Cod_Supervisor,
	Bloqueado,
	Per_ComAtoVen,
	Per_ComAtoCob,
	ISN_CTAFIN,
	COD_TABCOM,
	FLG_BLQVEN,
	VD.Cod_TipVenBas,
	Vlr_Objetivo,
	Cidade,
	Estado,
	Flg_Export
	FROM PROD_2023.dbo.VENDE VD
		INNER JOIN PROD_2023.dbo.VEXES ES ON VD.Codigo = ES.Cod_Vended
WHERE Cod_Estabe = 1
GO


