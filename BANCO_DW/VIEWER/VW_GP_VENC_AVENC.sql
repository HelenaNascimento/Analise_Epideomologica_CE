USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_GP_VENC_AVENC]    Script Date: 13/04/2024 16:26:00 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[VW_GP_VENC_AVENC] AS

SELECT
	DISTINCT
	GR.Des_GrpCli,
	count(distinct CL.CODIGO) Qtd_Cl,
	A1.vencido as Vencidos,
	A2.Avencer as A_Vencer
FROM PROD_2023.dbo.CLIEN CL
	inner join PROD_2023.dbo.ENXES ES  ON CL.Codigo = ES.Cod_Client
	inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
	inner join PROD_2023.dbo.CTREC ct ON  CL.Codigo = CT.Cod_Cliente
	INNER JOIN (SELECT 	distinct 
						GR.Cod_GrpCli AS Cod_GrpCli ,
						vencido = SUM(Vlr_Documento)
					FROM PROD_2023.dbo.CTREC ct
						inner join PROD_2023.dbo.CLIEN cl on ct.Cod_Cliente = cl.Codigo
						inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
						inner join PROD_2023.dbo.CIDAD cd on cl.Cod_Cidade = cd.Codigo
						inner join PROD_2023.dbo.VENDE ve on ct.Cod_Vendedor = ve.Codigo
					WHERE Cod_Estabe = 1
						and ct.Status = 'A'
						and Dat_Vencimento < GETDATE()
					GROUP BY GR.Cod_GrpCli) A1 ON GR.Cod_GrpCli = A1.Cod_GrpCli
	INNER JOIN (SELECT  distinct
						GR.Cod_GrpCli,
						Avencer = SUM(((Vlr_Documento * (Per_Juros / 100)) * cast(((getdate() - 1) - Dat_Vencimento)as int)) + Vlr_Documento)
				FROM PROD_2023.dbo.CTREC ct
						inner join PROD_2023.dbo.CLIEN cl on ct.Cod_Cliente = cl.Codigo
						inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
						inner join PROD_2023.dbo.CIDAD cd on cl.Cod_Cidade = cd.Codigo
						inner join PROD_2023.dbo.VENDE ve on ct.Cod_Vendedor = ve.Codigo
					where Cod_Estabe = 1
						and ct.Status = 'A'
						and Dat_Vencimento >= GETDATE()
					group by GR.Cod_GrpCli) A2 ON GR.Cod_GrpCli = A2.Cod_GrpCli
WHERE ES.Cod_Estabe = 1
	and ct.Status = 'A'


GROUP BY 
	GR.Des_GrpCli,
	A1.vencido,
	A2.Avencer

--select * from VW_GP_VENC_AVENC
GO


