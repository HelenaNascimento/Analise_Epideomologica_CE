USE [DMD_TESTE]
GO

/****** Object:  View [dbo].[VW_REL_ABC_FAB_PRC_ENTR]    Script Date: 01/07/2025 09:38:32 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[VW_REL_ABC_FAB_PRC_ENTR] AS 
SELECT 
	DISTINCT
	FB.FANTASIA AS Fabricante,
	MONTH(CB.Dat_Entrada) AS Mes_Entrada,
	DAY(Dat_Entrada) Dia_Entrada ,
	IT.Cod_Produto as Codigo,
	PR.Descri AS Descricao,
	Format(AVG(Prc_Unitario), 'c', 'pt-br') AS 'Prc_Unitario'
from NFEIT IT
		JOIN NFECB CB on it.cod_estabe = CB.Cod_Estabe and IT.Protocolo = CB.Protocolo
		JOIN PRODU PR ON IT.Cod_Produto = PR.CODIGO
		JOIN FABRI FB ON PR.Cod_Fabricante = FB.Codigo
	where 
		it.Cod_Estabe = 1
		and Tip_NF = 'C' 
		and MONTH(CB.Dat_Entrada) > month(getdate()) -6
		and MONTH(CB.Dat_Entrada) < month(getdate()) 
		and status not in ('A', 'C') 
	GROUP BY 	FB.FANTASIA, CB.Dat_Entrada, IT.Cod_Produto, PR.Descri
GO


