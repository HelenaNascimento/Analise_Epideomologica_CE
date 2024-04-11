USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_TVENDAS_VLRXDIA]    Script Date: 11/04/2024 17:15:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE VIEW [dbo].[DASH_TVENDAS_VLRXDIA]
AS

SELECT 
	year(Hor_Fatura) as Ano
	,month(Hor_Fatura) as Mes
	, day(Hor_Fatura) as Dia
	,format(C_VlrPedido, 'c', 'pt-br') as Valor_Venda
	FROM PROD_2023.dbo.PDVCB
WHERE Status1 = 'D' 
	and Status2 = 'D'
	and year(Hor_Fatura) >= '2023'
	and month(Hor_Fatura) >= '01'
	and day(Hor_Fatura) >= '01'

GO


