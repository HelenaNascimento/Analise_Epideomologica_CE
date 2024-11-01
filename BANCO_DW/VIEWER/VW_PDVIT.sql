USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_PDVIT]    Script Date: 13/04/2024 16:45:15 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_PDVIT] AS

SELECT 
	Cod_Pedido,
	Cod_Produto,
	Qtd_Pedido,
	Qtd_ImpFat,
	Vlr_Bruto,
	IT.C_VlrDesconto,
	C_VlrLiquido,
	Cod_ClaTri,
	Cod_Fabricante,
	Cod_Lote,
	IT.Cod_MtvRej,
	Cod_Promocao
	FROM PROD_2023.dbo.PDVIT IT
		inner join PROD_2023.dbo.PDVCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Cod_Pedido = CB.Numero
WHERE IT.Cod_Estabe = 1
	and Dat_Pedido >= '20230101'

GO


