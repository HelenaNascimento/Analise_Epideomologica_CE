USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_GRUP_FINANC]    Script Date: 11/04/2024 17:08:29 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[DASH_GRUP_FINANC] AS
SELECT 	
	Distinct
	GR.Des_GrpCli,
	vencido = SUM(Vlr_Documento),
	Dat_Vencimento
FROM PROD_2023.dbo.CTREC ct
	inner join PROD_2023.dbo.CLIEN cl on ct.Cod_Cliente = cl.Codigo
	inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
	inner join PROD_2023.dbo.CIDAD cd on cl.Cod_Cidade = cd.Codigo
	inner join PROD_2023.dbo.VENDE ve on ct.Cod_Vendedor = ve.Codigo
WHERE Cod_Estabe = 1
	and ct.Status = 'A'
GROUP BY GR.Des_GrpCli, Dat_Vencimento
GO


