USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_CLIENTE]    Script Date: 11/04/2024 17:22:07 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_CLIENTE] AS
SELECT
	CL.Codigo,
	CL.Razao_Social,
	CL.Fantasia,
	CL.Pessoa,
	CL.Cgc_Cpf,
	CL.Cgc_Matriz,
	CL.Cod_Estado AS 'UF',
	CD.Descricao AS 'CIDADE',
	Val_LicSau,
	Val_Anvisa,
	Val_CerReg,
	Val_AlvFun,
	Tipo_Consumidor,
	ES.Sta_ClaAbcVal,
	CL.Data_Cadastro,
	Limite_Credito,
	Dat_LimCreAtu,
	Vlr_LimCreAnt,
	Total_Debito,
	CL.Data_UltimaFatura,
	CL.Valor_UltimaFatura,
	CL.Valor_MaiorFatura,
	Maior_Atraso,
	ES.Qtd_PrzMax,
	Isn_CtaFin,
	Cod_RamoAtividade,
	RA.Descricao,
	Bloqueado,
	Motivo_Bloqueio,
	Observacao,
	Cod_GrpCli,
	Flg_BlqVenOrc,
	Tip_DscPdv,
	cl.Qtd_MesMinPrzVctLot,
	Cod_Rota,
	Cod_RegTri,
	Cod_AgeCob,
	Cod_Vendedor,
	Cod_Operador
	FROM PROD_2023.dbo.CLIEN CL
		INNER JOIN PROD_2023.dbo.ENXES ES ON CL.Codigo = ES.COD_CLIENT
		LEFT JOIN PROD_2023.dbo.ESTAD ET ON CL.Cod_Estado = ET.Codigo
		LEFT JOIN PROD_2023.dbo.CIDAD CD ON CL.Cod_Cidade = CD.Codigo AND ET.Codigo = CD.Cod_Estado AND CL.Cod_Estado = ET.Codigo
		LEFT JOIN PROD_2023.dbo.BAIRR BA ON CL.Cod_Bairro = BA.Codigo AND CD.Codigo = BA.Cod_Cidade AND ET.Codigo = BA.Cod_Estado
		LEFT JOIN PROD_2023.dbo.RMATV RA ON CL.Cod_RamoAtividade = RA.Codigo
WHERE
		ES.Cod_Estabe = 1
GO


