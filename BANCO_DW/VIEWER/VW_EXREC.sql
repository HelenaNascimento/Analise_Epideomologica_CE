USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_EXREC]    Script Date: 11/04/2024 17:29:49 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_EXREC] AS

SELECT 
	DatCtb,
	LocCtb,
	QtdLan,
	ValPri,
	ValJur,
	ValDsc,
	ValLan,
	NomArq
	FROM PROD_2023.dbo.EXREC
where Cod_Estabe = 1
	and DatCtb >= '20220101'

GO


