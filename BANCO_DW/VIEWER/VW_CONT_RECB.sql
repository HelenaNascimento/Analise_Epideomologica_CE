USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_CONT_RECB]    Script Date: 11/04/2024 17:25:01 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_CONT_RECB] as

SELECT
	DISTINCT
	CL.CODIGO,
	cl.Razao_Social,
	sum(Qtd_DiaExtVct) as Qtd_DiaExtVct,
	GR.Des_GrpCli,
	SUM(A1.vencido) as Vencidos,
	SUM(A2.Avencer) as A_Vencer,
	SUM(A3.Quitado) as Quitado
FROM PROD_2023.dbo.CLIEN CL
	inner join PROD_2023.dbo.ENXES ES  ON CL.Codigo = ES.Cod_Client
	inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
	inner join PROD_2023.dbo.CTREC ct ON  CL.Codigo = CT.Cod_Cliente
	LEFT JOIN (SELECT 	distinct 
						CL.CODIGO,
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
					GROUP BY GR.Cod_GrpCli, cl.codigo) A1 ON GR.Cod_GrpCli = A1.Cod_GrpCli and  ct.Cod_Cliente = A1.Codigo
	LEFT JOIN (SELECT  distinct
						CL.CODIGO,
						GR.Cod_GrpCli,
						Avencer = SUM(Vlr_Documento)
				FROM PROD_2023.dbo.CTREC ct
						inner join PROD_2023.dbo.CLIEN cl on ct.Cod_Cliente = cl.Codigo
						inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
						inner join PROD_2023.dbo.CIDAD cd on cl.Cod_Cidade = cd.Codigo
						inner join PROD_2023.dbo.VENDE ve on ct.Cod_Vendedor = ve.Codigo
					where Cod_Estabe = 1
						and ct.Status = 'A'
						and Dat_Vencimento >= GETDATE()
					group by GR.Cod_GrpCli, cl.codigo) A2 ON GR.Cod_GrpCli = A2.Cod_GrpCli and  ct.Cod_Cliente = A2.Codigo
	LEFT JOIN (SELECT  distinct
					cl.codigo,
					GR.Cod_GrpCli,
					Quitado = SUM(Vlr_Documento)
			FROM PROD_2023.dbo.CTREC ct
					inner join PROD_2023.dbo.CLIEN cl on ct.Cod_Cliente = cl.Codigo
					inner join PROD_2023.dbo.GRCLI GR ON CL.Cod_GrpCli = GR.Cod_GrpCli
					inner join PROD_2023.dbo.CIDAD cd on cl.Cod_Cidade = cd.Codigo
					inner join PROD_2023.dbo.VENDE ve on ct.Cod_Vendedor = ve.Codigo
				where Cod_Estabe = 1
					and ct.Status = 'Q'
					and Dat_Vencimento < GETDATE()
				group by GR.Cod_GrpCli, cl.codigo) A3 ON GR.Cod_GrpCli = A2.Cod_GrpCli and  ct.Cod_Cliente = A3.Codigo
WHERE ES.Cod_Estabe = 1
and cl.Bloqueado= 0

group by 	
	CL.CODIGO,
	cl.Razao_Social,
	GR.Des_GrpCli
	
GO


