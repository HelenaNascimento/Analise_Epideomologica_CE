USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_OR_PEDIDO]    Script Date: 13/04/2024 16:29:53 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE VIEW [dbo].[VW_OR_PEDIDO] AS
select 
	DISTINCT
	cb.Cod_Vendedor,
	ve.Nome_Guerra,
	Or_Pedido = case
		when Cod_OrigemPdv = 'AL' then 'Ativo'
		when Cod_OrigemPdv = 'ML' then 'Móvel'
		when Cod_OrigemPdv = 'TL' then 'OL'
		--else Cod_OrigemPdv
		end,
	count(numero) as Qtd_Pedido,
	sum(C_VlrPedido) VLR_PEDI,
	month(Dat_Pedido) as Mes,
	year(Dat_Pedido) as Ano
	from PROD_2023.dbo.PDVCB  cb
			inner join PROD_2023.dbo.VENDE ve on cb.Cod_Vendedor = ve.codigo
	where Cod_Estabe = 1
	and Status2 = 'D'
	AND VE.Codigo NOT IN (464, 472)
	group by 
	cb.Cod_Vendedor,
	ve.Nome_Guerra,
	Cod_OrigemPdv,
	Dat_Pedido
GO


