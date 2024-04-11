USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_FIN_CLIEN_INAD]    Script Date: 11/04/2024 17:33:28 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE VIEW [dbo].[VW_FIN_CLIEN_INAD] AS

SELECT 
	DISTINCT
	month(cl.Data_UltimaFatura) as mes,
	CL.Codigo,
	CL.Razao_Social,
	CL.Cgc_Cpf,
	cl.Bloqueado,
	cl.Motivo_Bloqueio,
	C_A.Dias_Atraso,
	C_A.qtd_bol_aber,
	C_A.Vlr_Documento as Vlr_Documento,
	C_a.Vlr_Atual as Vlr_Atual,
	CL.Data_UltimaFatura,
	cl.Valor_UltimaFatura
	
  FROM (SELECT 
			Cgc_Cpf, 
			Dias_Atraso as Dias_Atraso, 
			count(Num_Documento) as qtd_bol_aber, 
			sum(Vlr_Documento) as Vlr_Documento, 
			sum(Vlr_Atual) as Vlr_Atual  
		FROM [DW_PROD].[dbo].[VW_FIN_BOL_A_] 
		group by Cgc_Cpf, Dias_Atraso ) C_A
	INNER JOIN PROD_2023.dbo.CLIEN CL ON C_A.Cgc_Cpf = CL.Cgc_Cpf
	INNER JOIN PROD_2023.dbo.ENXES ES ON CL.Codigo = ES.Cod_Client
  WHERE 
	ES.Cod_Estabe = 1 and
	YEAR(CL.Data_UltimaFatura) = year(getdate()) and
	month (cl.Data_UltimaFatura) = month(getdate()) - 1

GO


