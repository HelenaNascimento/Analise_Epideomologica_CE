USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_RECEB]    Script Date: 13/04/2024 16:48:15 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_RECEB] AS
SELECT 
	Cod_rec,
	Data,
	Receber_de,
	Rec_tip,
	Tot_tit,
	Tot_des,
	Tot_acre,
	Tot_ger,
	Rec_esp,
	Rec_Val,
	Rec_Dep,
	Rec_Cre,
	Pag_Cre,
	Pag_ChqDev,
	Cod_CliPag,
	Cod_Vended,
	Transacao,
	Cod_Banco,
	Num_Retorno
	FROM PROD_2023.dbo.RECEB
WHERE Cod_Estabe = 1
	AND Data >='20220101'

/*
SELECT TOP 10 * FROM PROD_2023.dbo.CRECL

SELECT TOP 10 * FROM PROD_2023.dbo.EXREC
*/
GO


