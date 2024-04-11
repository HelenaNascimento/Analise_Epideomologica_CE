USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_BXREC]    Script Date: 11/04/2024 17:21:28 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_BXREC] AS
SELECT 
	Cod_Documento,
	Cod_Lancamento,
	Status,
	Dat_Lancamento,
	Dat_Registro,
	Dat_Caixa,
	Tip_Baixa,
	Tip_Doc,
	Qtd_DiasAtraso,
	Vlr_Principal,
	Vlr_Desconto,
	Vlr_Deducoes,
	Vlr_Juros,
	Vlr_Acrescimos,
	Sld_Principal,
	Vlr_JurCalc,
	Per_Juros,
	Vlr_DebVdr,
	Cod_Rec,
	Transacao,
	Cod_CtrOri,
	Cod_CabLanFin,
	Vlr_Multa,
	Vlr_ComPag,
	Dat_Cancel,
	Isn_CtaFin,
	Cod_CntCus,
	Vlr_DspCartorio,
	Cod_LanDep,
	Cod_Vale,
	Val_ValeUtilizado,
	Observacao,
	Id_Transacao
	FROM PROD_2023.dbo.BXREC
where Cod_Estabe = 1
	and Dat_Lancamento = '20220101'
/*
SELECT TOP 10 * FROM PROD_2023.dbo.RECEB

SELECT TOP 10 * FROM PROD_2023.dbo.CRECL

SELECT TOP 10 * FROM PROD_2023.dbo.EXREC
*/
GO


