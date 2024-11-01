USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_CTREC]    Script Date: 11/04/2024 17:25:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_CTREC] AS

--CTREC
--BXREC

SELECT 
	Cod_Documento, 
	Tip_Documento, 
	Cod_Banco, 
	Num_Documento, 
	Par_Documento, 
	Status,
	Tip_Vencimento,
	Tip_Cobranca,
	Cod_OrigemCtr,
	Dat_Emissao,
	Dat_BaseJuros,
	Dat_Vencimento,
	Qtd_DiaExtVct,
	Dat_Quitacao,
	Per_DscAtc,
	Vlr_DscAtc,
	Cod_Cliente,
	Cgc_Matriz,
	Cod_Agente,
	Cod_Vendedor,
	Cod_VendTlmkt,
	Ser_NfOrigem,
	Num_NfOrigem,
	Cod_ServRem,
	Dat_Remessa,
	Num_Remessa,
	Cod_ServRet,
	Dat_Retorno,
	Num_Retorno,
	Per_Juros,
	Per_DescFinanc,
	Vlr_DescConced,
	Vlr_DscSubFat,
	Vlr_Documento,
	Vlr_Comissao,
	Vlr_ComTlmkt,
	Vlr_ComPag,
	Vlr_ComPagTlmkt,
	Per_MulAtrPag,
	Dat_Carta,
	Dat_Negociacao,
	Dat_Cartorio,
	Dat_Prorrogacao,
	Dat_Protesto,
	Dat_Juridico,
	Vlr_ParTit,
	Vlr_OutAcr,
	Isn_CtaFin,
	Cod_CntCus,
	Isn_CtaFinCre,
	Dat_Cancel,
	CodAnt,
	NovoCodigo,
	Vlr_DscBonDup
FROM PROD_2023.dbo.CTREC
where Cod_Estabe = 1
	and Dat_Registro >= '20220101'

/*
SELECT TOP 10 * FROM PROD_2023.dbo.BXREC

SELECT TOP 10 * FROM PROD_2023.dbo.RECEB

SELECT TOP 10 * FROM PROD_2023.dbo.CRECL

SELECT TOP 10 * FROM PROD_2023.dbo.EXREC
*/
GO


