USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_ENTRA_S_SAIDA]    Script Date: 11/04/2024 17:26:00 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_ENTRA_S_SAIDA] AS
SELECT 
	Cod_Fabricante as Codigo,
	FB.Fantasia as Fantasia,
	eit.Cod_Produto, 
	sum(eit.Prc_UniFat * eit.Qtd_Pedido) as total_ent,
	IsNull(Entr.total_Saida, 0) as total_Saida, 
	
	year(ECB.DAT_ENTRADA) as ano,
	month(ECB.DAT_ENTRADA) as mes
FROM PROD_2023.dbo.NFEIT EIT
	INNER JOIN PROD_2023.dbo.NFECB ECB ON EIT.Cod_Estabe = ECB.Cod_Estabe AND EIT.PROTOCOLO = ECB.PROTOCOLO
	INNER JOIN PROD_2023.dbo.PRODU PR ON EIT.Cod_Produto = PR.Codigo
	INNER JOIN PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
	left join (SELECT 
					IT.Cod_Estabe,
					it.Cod_Produto, 
					sum(it.Qtd_Produto) as total_Saida,
					year(CB.Dat_Emissao) as ano,
					month(CB.Dat_Emissao) as mes
					FROM PROD_2023.dbo.NFSIT IT 
							INNER JOIN PROD_2023.dbo.NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe and IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota  
							INNER JOIN PROD_2023.dbo.PRODU PR ON IT.Cod_Produto = PR.Codigo
							INNER JOIN PROD_2023.dbo.FABRI FB ON PR.Cod_Fabricante = FB.Codigo
				WHERE 
					IT.Cod_Estabe = 1
				GROUP BY 
					IT.Cod_Estabe,
					it.Cod_Produto, 
					CB.Dat_Emissao
					) as Entr on EIT.Cod_Estabe = entr.Cod_Estabe and EIT.Cod_Produto = entr.Cod_Produto  and year(ECB.Dat_Entrada) = entr.ano and month(ECB.Dat_Entrada) = entr.mes

WHERE
	EIT.Cod_Estabe = 1
and year(ECB.Dat_Entrada) >= year(getdate()) -1
AND EIT.Cod_Cfo in (1910, 2910, 2102, 2403, 2404)

GROUP BY
	Cod_Fabricante,
	Fantasia,
	EIT.Cod_Produto,
	entr.total_Saida,
	DAT_ENTRADA
GO


