USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_CLIENTE_DH]    Script Date: 11/04/2024 17:24:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE VIEW [dbo].[VW_CLIENTE_DH] AS
SELECT
	CL.Codigo,
	CL.Cod_Estado AS 'UF',
	CD.Descricao AS 'CIDADE',
	CD.Cod_CidIbge,
	MONTH (Val_LicSau) AS Val_LicSau,
	MONTH (Val_Anvisa) AS Val_Anvisa,
	MONTH (Val_CerReg) AS Val_CerReg,
	MONTH (Val_AlvFun) AS Val_AlvFun,
	ES.Sta_ClaAbcVal,
	CL.Valor_UltimaFatura,
	Cod_Vendedor,
	YEAR (CL.Data_UltimaFatura) AS ANO,
	MONTH (CL.Data_UltimaFatura) AS MES
	FROM PROD_2023.dbo.CLIEN CL
		INNER JOIN PROD_2023.dbo.ENXES ES ON CL.Codigo = ES.COD_CLIENT
		LEFT JOIN PROD_2023.dbo.ESTAD ET ON CL.Cod_Estado = ET.Codigo
		LEFT JOIN PROD_2023.dbo.CIDAD CD ON CL.Cod_Cidade = CD.Codigo AND ET.Codigo = CD.Cod_Estado AND CL.Cod_Estado = ET.Codigo
		LEFT JOIN PROD_2023.dbo.BAIRR BA ON CL.Cod_Bairro = BA.Codigo AND CD.Codigo = BA.Cod_Cidade AND ET.Codigo = BA.Cod_Estado
		LEFT JOIN PROD_2023.dbo.RMATV RA ON CL.Cod_RamoAtividade = RA.Codigo
WHERE
		ES.Cod_Estabe = 1
	AND CL.Data_UltimaFatura < GETDATE() - 1 
	AND YEAR (CL.Data_UltimaFatura) >= YEAR(GETDATE()) - 1 
	AND Bloqueado = 0
GO


