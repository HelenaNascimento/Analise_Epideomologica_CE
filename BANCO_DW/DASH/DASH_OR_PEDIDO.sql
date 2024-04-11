USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_OR_PEDIDO]    Script Date: 11/04/2024 17:09:12 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




/****** Script do comando SelectTopNRows de SSMS  ******/

CREATE VIEW [dbo].[DASH_OR_PEDIDO] as
SELECT
		Cod_Vendedor
		,[Nome_Guerra]
      ,[Or_Pedido]
      ,sum([Qtd_Pedido]) AS [Qtd_Pedido]
      ,sum([VLR_PEDI]) AS [VLR_PEDI]
      ,[Mes]
      ,[Ano]
  FROM [DW_PROD].[dbo].[VW_OR_PEDIDO]
  GROUP BY 
		[Or_Pedido],
		[Mes],
        [Ano],
		[Nome_Guerra],
		Cod_Vendedor
		
GO


