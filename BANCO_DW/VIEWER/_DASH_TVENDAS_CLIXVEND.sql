USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_TVENDAS_CLIXVEND]    Script Date: 27/06/2024 13:31:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE VIEW [dbo].[DASH_TVENDAS_CLIXVEND] AS 

SELECT 
	year(Hor_Fatura) as ano,
	month(Hor_Fatura) as mes,
	day(Hor_Fatura) as dia,
	count(Cod_Cliente) as Qtd_Cliente,
	day(Hor_Fatura) as dias
	FROM PROD_2023.dbo.PDVCB
WHERE Status1 = 'D' 
	and Status2 = 'D'
	and year(Hor_Fatura) >= '2023'
	and month(Hor_Fatura) >= '01'
	and day(Hor_Fatura) >= '01'
group by 
	year(Hor_Fatura) ,
	month(Hor_Fatura),
	day(Hor_Fatura) 

GO


